
n <- 20; p <- 2; B <- 100; normalize <- FALSE; return_resamples <- FALSE
data <- list(X = rbinom(n = n, size = 1, prob = 0.2),
             Y = rpois(n = n, lambda = 1),
             Z = matrix(rnorm(n = n*p, mean = 0, sd = 1), nrow = n, ncol = p))
X <- data$X; Y <- data$Y; Z <- data$Z
X_on_Z_fam <- "binomial"
Y_on_Z_fam <- "poisson"

X_on_Z_fit <- stats::glm(X ~ Z, family = X_on_Z_fam)
Y_on_Z_fit <- stats::glm(Y ~ Z, family = Y_on_Z_fam)



sp.approx.binomial <- function(t, data, X_on_Z_fit, Y_on_Z_fit, normalize = FALSE){
   
   X <- data$X; Y <- data$Y; Z <- data$Z
   n <- length(X)
   
   W <- Y - Y_on_Z_fit$fitted.values
   # P <- stats::rbinom(n = n, size = 1, prob = X_on_Z_fit$fitted.values)
   P <- X_on_Z_fit$fitted.values
   
   wcgf <- function(s, P, W){
      n <- length(P)
      return(sum(log(exp(s*W)*P + 1 - P)))
   }
   
   # weighted derivative of cgf
   d1.wcgf <- function(s, P, W){
      n <- length(P)
      Q <- 1 - P
      # sum((W*P*exp(s*W)) / (exp(s*W)*P + 1 - P))
      return(sum(W - (W*Q)/(exp(s*W)*P + Q)))
   }
   
   # weighted Hessian of cgf
   d2.wcgf <- function(s, P, W){
      n <- length(P)
      return(sum((W^2*P*Q*exp(s*W)) / (exp(s*W)*P + Q)^2))
   }
   
   s.hat <- uniroot(function(s){d1.wcgf(s, P, W) - t}, 
                   lower = -20, upper = 20)$root
   
   r.hat <- sign(s.hat) * sqrt(2 * (n*s.hat*t - wcgf(s = s.hat, P = P, W = W)))
   
   F.hat <- pnorm(r.hat) + dnorm(r.hat) * 
                     (1/r.hat - 1/(s.hat*sqrt(d2.wcgf(s = s.hat, P = P, W = W))))
   return(F.hat)
}




