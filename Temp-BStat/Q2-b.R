library(mvtnorm)

log.lh <- function(X, theta){
   N <- nrow(X)
   
   p <- theta[[1]]
   q <- 1 - p
   mu1 <- theta[[2]]
   mu2 <- theta[[3]]
   mu3 <- theta[[4]]
   sigma1 <- theta[[5]]
   sigma2 <- theta[[6]]
   sigma3 <- theta[[7]]
   ind.1 <- theta[[8]]
   ind.2 <- theta[[9]]
   ind.3 <- theta[[10]]
   
   return(2*log(p)*sum(ind.1) + log(2*p*q)*sum(ind.2) + 2*log(q)*sum(ind.3) +
             sum(sapply(1:N, function(val){
                return(ind.1[val] * dmvnorm(X[val, ], mu = mu1, Sigma = sigma1, log = T))
             })) +
             sum(sapply(1:N, function(val){
                return(ind.2[val] * dmvnorm(X[val, ], mu = mu2, Sigma = sigma2, log = T))
             })) +
             sum(sapply(1:N, function(val){
                return(ind.3[val] * dmvnorm(X[val, ], mu = mu3, Sigma = sigma3, log = T))
             })))
}


em.est <- function(X, theta){
   
   p <- theta[[1]]
   mu1 <- theta[[2]]
   mu2 <- theta[[3]]
   mu3 <- theta[[4]]
   sigma1 <- theta[[5]]
   sigma2 <- theta[[6]]
   sigma3 <- theta[[7]]
   # old.ind.1 <- theta[[8]]
   # old.ind.2 <- theta[[9]]
   # old.ind.3 <- theta[[10]]
   
   N <- nrow(X)
   
   alpha.1 <- p^2
   alpha.2 <- 2*p*(1-p)
   alpha.3 <- (1-p)^2
   
   S <- sapply(1:N, function(val){
      return(alpha.1 * dmvnorm(X[val, ], mu = mu1, Sigma = sigma1) +
         alpha.2 * dmvnorm(X[val, ], mu = mu2, Sigma = sigma2) +
         alpha.3 * dmvnorm(X[val, ], mu = mu3, Sigma = sigma3))
   })
   
   ind.1 <- sapply(1:N, function(val){
      return(alpha.1 * dmvnorm(X[val, ], mu = mu1, Sigma = sigma1) / S[val])
   })
   
   ind.2 <- sapply(1:N, function(val){
      return(alpha.2 * dmvnorm(X[val, ], mu = mu2, Sigma = sigma2) / S[val])
   })
   
   ind.3 <- sapply(1:N, function(val){
      return(alpha.3 * dmvnorm(X[val, ], mu = mu3, Sigma = sigma3) / S[val])
   })   
      
   new.p <- (2 * sum(ind.1) + sum(ind.2)) / (2*N)
   
   new.mu1 <- rowSums(sapply(1:N, function(val) ind.1[val] * X[val, ])) / sum(ind.1)
   new.mu2 <- rowSums(sapply(1:N, function(val) ind.2[val] * X[val, ])) / sum(ind.2)
   new.mu3 <- rowSums(sapply(1:N, function(val) ind.3[val] * X[val, ])) / sum(ind.3)
   
   J <- list()
   for(val in 1:N){
      J[[val]] <- ind.1[val] * as.matrix(X[val, ] - new.mu1) %*% (X[val, ] - new.mu1)
   }
   new.sigma1 <- Reduce('+', J) / sum(ind.1)
   
   J <- list()
   for(val in 1:N){
      J[[val]] <- ind.2[val] * as.matrix(X[val, ] - new.mu2) %*% (X[val, ] - new.mu2)
   }
   new.sigma2 <- Reduce('+', J) / sum(ind.2)
   
   J <- list()
   for(val in 1:N){
      J[[val]] <- ind.3[val] * as.matrix(X[val, ] - new.mu3) %*% (X[val, ] - new.mu3)
   }
   new.sigma3 <- Reduce('+', J) / sum(ind.3)
   
   new.theta <- list(new.p, new.mu1, new.mu2, new.mu3,
                     new.sigma1, new.sigma2, new.sigma3,
                     ind.1, ind.2, ind.3)
      
   return(new.theta)
}

N <- 100
d <- 4
p <- 0.3
K <- floor(N/3)
tol <- 1E-5
X <- matrix(0, nrow = N, ncol = d)
U <- runif(N)

init.mu1 <- c(0,0,0,0)
init.mu2 <- c(1,1,4,-2)
init.mu3 <- c(0,1,3,0)

init.sigma1 <- diag(c(1,1,1,1))
init.sigma2 <- diag(c(1,2,0.5,1))
init.sigma3 <- diag(c(2,4,1,4))

init.ind.1 <- c(rep(1,K),rep(0,N-K))
init.ind.2 <- c(rep(0,K),rep(1,K),rep(0,N-2*K))
init.ind.3 <- c(rep(0,N-K),rep(1,K))

#Sampling from the mixture
for(i in 1:N){
   if(U[i] <= p^2){
      X[i,] <- rmvnorm(1, mean = init.mu1, sigma = init.sigma1)
   }else if(U[i] < 1 - (1-p)^2){
      X[i,] <- rmvnorm(1, mean = init.mu2, sigma = init.sigma2)
   }else{
      X[i,] <- rmvnorm(1, mean = init.mu3, sigma = init.sigma3)
   }
}

difference <- 10
count <- 1

# theta <- list(p, init.mu1, init.mu2, init.mu3,
#               init.sigma1, init.sigma2, init.sigma3,
#               init.ind.1, init.ind.2, init.ind.3)

theta <- list(p, init.mu1, init.mu2, init.mu3,
              init.sigma1, init.sigma2, init.sigma3,
              init.ind.1, init.ind.2, init.ind.3)

while(difference > tol){
   new.theta <- em.est(X, theta)
   print(count)
   difference <- abs(log.lh(X, new.theta) - log.lh(X, theta))
   print(difference)
   
   if(difference <= tol){
      print(new.theta)
      break
   }
   
   theta <- new.theta
   count <- count + 1
}

