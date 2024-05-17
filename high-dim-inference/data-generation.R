
library(magrittr)
library(MASS)

n <- 50
p <- 80

## Model 1

M <- R <- matrix(sample.int(p^2, size = p^2, replace = F), 
                 nrow = p, ncol = p)

apply(M, 1:2, function(val){
   pos <- which(M == val, arr.ind = TRUE)
   R[pos] <- 0.6^abs(pos[1] - pos[2])
}) -> R_1

R_1


## Model 2

M <- R <- matrix(sample.int(p^2, size = p^2, replace = F), 
                 nrow = p, ncol = p)

apply(M, 1:2, function(val){
   pos <- which(M == val, arr.ind = TRUE)
   
   if(pos[1] != pos[2]){
      R[pos] <- rbinom(n = 1, size = 1, prob = 0.2) * 1
   }else{
      R[pos] <- 0
   }
}) -> C

B <- 0.5 * (C+t(C))
B[B == 1] <- 0.5

R_2 <- B + diag(1,p,p) * (max(eigen(B)$values) - p*min(eigen(B)$values)) / (p-1)

R_2


## Model 3

R_3 <- matrix(0.5, nrow = p, ncol = p) + 0.5 * diag(1,p,p)

R_3


##################################

X_1 <- mvrnorm(n = n, mu = rep(0,p), Sigma = solve(R_1))
X_2 <- mvrnorm(n = n, mu = rep(0,p), Sigma = solve(R_2))
X_3 <- mvrnorm(n = n, mu = rep(0,p), Sigma = solve(R_3))

















