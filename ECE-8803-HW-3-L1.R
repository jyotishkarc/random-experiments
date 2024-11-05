library(MASS)
library(ggplot2)
library(latex2exp)
library(tictoc)

proj_sparse_L1 <- function(x, tau) {
   if (sum(abs(x)) <= tau) {
      return(x)
   }
   
   u <- sort(abs(x), decreasing = TRUE)
   sv <- cumsum(u) - tau
   
   rho <- max(which(u > sv / seq_along(u)))
   theta <- sv[rho] / rho
   
   x_proj <- sign(x) * pmax(abs(x) - theta, 0)
   
   return(x_proj)
}


####

# projected GD
proj_GD_L1 <- function(X, y, b, theta_0, eta, s, T_max = 100){
   
   theta <- list()
   theta[[1]] <- proj_sparse_L1(theta_0 + 2*eta*t(X) %*% (y - X %*% theta_0), s)
   
   error <- c()
   error[1] <- log(sqrt(sum((theta[[1]] - as.matrix(b))^2)))
   
   for(h in 1:(T_max-1)) {
      
      temp <- theta[[h]] + 2*eta*t(X) %*% (y - X %*% theta[[h]])
      theta[[h+1]] <- proj_sparse_L1(temp, s)
      
      error[h+1] <- log(sqrt(sum((theta[[h+1]] - as.matrix(b))^2)))
      
      print(h)
   }
   
   plot_title <- paste0("Constrained L1 norm: Error plot for n = ",nrow(X),
                        ", d = ", ncol(X),
                        ", s = ",s)
   
   error_plot <- data.frame(t = 1:T_max, 
                            log_err = error) |> 
      ggplot(aes(x = t, y = log_err)) +
      geom_line(color = 'red', linewidth = 0.7) +
      xlab(TeX("\\textit{t}")) +
      ylab(TeX("$\\log\\left(||\\theta_{\\textit{t}} - \\theta^{*}||_2\\right)$")) +
      theme_minimal() +
      ggtitle(plot_title) +
      theme(plot.title = element_text(hjust = 0.5, face = 'bold'))
   
   print(error_plot)
   
   return(list(theta = theta, 
               error = error,
               plot = error_plot))
}


##########

n <- 2500
d <- 5000
s <- floor(sqrt(d))

{
   tic()
   X_5k_parts <- list()
   for (i in 1:10) {
      X_5k_parts[[i]] <- MASS::mvrnorm(n, mu = rep(0,d/10), 
                                       Sigma = diag(rep(1,d/10)))
      print(i)
   }
   
   X_5k <- do.call(cbind, X_5k_parts)
   rm(X_5k_parts)
   toc()
}

g <- rnorm(d)
g[sample(1:d, d-s)] <- 0
b_5k <- g/sqrt(sum(g^2))

y_5k <- X_5k %*% as.matrix(b_5k) + 1/2 * MASS::mvrnorm(n, mu = 0, Sigma = 1)

theta_0 = matrix(0, d, 1)

res <- proj_GD_L1(X_5k, y_5k, b_5k,
                  theta_0 = theta_0, 
                  eta = 1e-6,
                  s = s)


##########

n <- 2500
d <- 10000
(s <- floor(sqrt(d)))

{
   tic()
   X_10k_parts <- list()
   for (i in 1:10) {
      X_10k_parts[[i]] <- MASS::mvrnorm(n, mu = rep(0,d/10), 
                                        Sigma = diag(rep(1,d/10)))
      print(i)
   }
   
   X_10k <- do.call(cbind, X_10k_parts)
   rm(X_10k_parts)
   toc()
}

g <- rnorm(d)
g[sample(1:d, d-s)] <- 0
b_10k <- g/sqrt(sum(g^2))

y_10k <- X_10k %*% as.matrix(b_10k) + 1/2 * MASS::mvrnorm(n, mu = 0, Sigma = 1)

theta_0 = matrix(0, d, 1)

res <- proj_GD_L1(X_10k, y_10k, b_10k,
                  theta_0 = theta_0, 
                  eta = 1e-6,
                  s = s)


##########

n <- 2500
d <- 20000
(s <- floor(sqrt(d)))

{
   tic()
   X_20k_parts <- list()
   for (i in 1:10) {
      X_20k_parts[[i]] <- MASS::mvrnorm(n, mu = rep(0,d/10), 
                                        Sigma = diag(rep(1,d/10)))
      print(i)
   }
   
   X_20k <- do.call(cbind, X_20k_parts)
   rm(X_20k_parts)
   toc()
}

g <- rnorm(d)
g[sample(1:d, d-s)] <- 0
b_20k <- g/sqrt(sum(g^2))

y_20k <- X_20k %*% as.matrix(b_20k) + 1/2 * MASS::mvrnorm(n, mu = 0, Sigma = 1)

theta_0 = matrix(0, d, 1)

res <- proj_GD_L1(X_20k, y_20k, b_20k,
                  theta_0 = theta_0, 
                  eta = 1e-6,
                  s = s)








