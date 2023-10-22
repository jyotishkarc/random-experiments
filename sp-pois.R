
n <- 20; p <- 2; B <- 100; normalize <- FALSE; return_resamples <- FALSE
data <- list(X = rpois(n = n, lambda = 2),
             Y = rnorm(n = n),
             Z = matrix(rnorm(n = n*p, mean = 0, sd = 1), nrow = n, ncol = p))
X <- data$X; Y <- data$Y; Z <- data$Z
X_on_Z_fam <- "poisson"
Y_on_Z_fam <- "gaussian"

X_on_Z_fit <- stats::glm(X ~ Z, family = X_on_Z_fam)
Y_on_Z_fit <- stats::glm(Y ~ Z, family = Y_on_Z_fam)



sp.approx.poisson <- function(t, data, X_on_Z_fit, Y_on_Z_fit, normalize = FALSE){
   
   X <- data$X; Y <- data$Y; Z <- data$Z
   n <- length(X)
   
   W <- Y - Y_on_Z_fit$fitted.values
   # P <- stats::rbinom(n = n, size = 1, prob = X_on_Z_fit$fitted.values)
   P <- X_on_Z_fit$fitted.values
   
   wcgf <- function(s, P, W){
      n <- length(P)
      return(sum(P*(exp(s*W)- 1)))
   }
   
   # weighted derivative of cgf
   d1.wcgf <- function(s, P, W){
      n <- length(P)
      return(sum(W*P*exp(s*W)))
      # return(sum(W - (W*Q)/(exp(s*W)*P + Q)))
   }
   
   # weighted Hessian of cgf
   d2.wcgf <- function(s, P, W){
      n <- length(P)
      return(sum(W^2*P*exp(s*W)))
   }
   
   s.hat <- uniroot(function(s){d1.wcgf(s, P, W) - t}, 
                    lower = -20, upper = 20)$root
   
   r.hat <- sign(s.hat) * sqrt(2 * (n*s.hat*t - wcgf(s = s.hat, P = P, W = W)))
   
   F.hat <- pnorm(r.hat) + dnorm(r.hat) * 
      (1/r.hat - 1/(s.hat*sqrt(d2.wcgf(s = s.hat, P = P, W = W))))
   
   return(F.hat)
   
   
   
   
   
   
   
   
   
   
   
}




