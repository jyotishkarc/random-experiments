
library('magrittr')
library('MASS')
library('psych')

library('flare')
library('FastBandChol')

n <- 30

## Model 1

P_1 <- function(p, seed){
   set.seed(seed)
   
   M <- R <- matrix(sample.int(p^2, size = p^2, replace = F), 
                    nrow = p, ncol = p)
   
   apply(M, 1:2, function(val){
      pos <- which(M == val, arr.ind = TRUE)
      R[pos] <- 0.6^abs(pos[1] - pos[2])
   }) -> R_1
   
   return(R_1)
}


## Model 2

P_2 <- function(p, seed){
   set.seed(seed)
   
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
   R_2 <- B + diag(1,p,p) * (max(eigen(B)$values)-p*min(eigen(B)$values)) / (p-1)
   
   return(R_2)
}


## Model 3

P_3 <- function(p, seed){
   set.seed(seed)
   
   R_3 <- matrix(0.5, nrow = p, ncol = p) + 0.5 * diag(1,p,p)
   
   return(R_3)
}


################################################ CLIME

L <- function(S,O){
   return(psych::tr(t(O) %*% S) - log(det(O)))
}

clime.res <- function(data, R_i){
   out <- sugm(data, method = "clime", nlambda = 25)
   m <- out$icov %>% 
      lapply(function(O) L(solve(R_i),O)) %>% 
      unlist() %>% which.min()
   
   return(out$icov[[m]])
}

########## CLIME Implementation

frob.CLIME.mat <- matrix(NA, nrow = 4, ncol = 2)
oper.CLIME.mat <- matrix(NA, nrow = 4, ncol = 2)

for(p in c(30,60)){
   
   frob.CLIME <- oper.CLIME <- matrix(NA, nrow = 4, ncol = 3)
   
   for(seed in 1:3){
      R_1 <- P_1(p,seed)
      R_2 <- P_2(p,seed)
      R_3 <- P_3(p,seed)
      
      X_1 <- mvrnorm(n = n, mu = rep(0,p), Sigma = solve(R_1))
      X_2 <- mvrnorm(n = n, mu = rep(0,p), Sigma = solve(R_2))
      X_3 <- mvrnorm(n = n, mu = rep(0,p), Sigma = solve(R_3))
      X_4 <- mvrnorm(n = n, mu = rep(0,p), Sigma = R_1)

            
      ## 1
      
      frob.CLIME[1,seed] <- norm(clime.res(X_1, R_1)-R_1, type = "F")
      oper.CLIME[1,seed] <- norm(clime.res(X_1, R_1)-R_1, type = "2")
      
      ## 2
      
      frob.CLIME[2,seed] <- norm(clime.res(X_2, R_2)-R_2, type = "F")
      oper.CLIME[2,seed] <- norm(clime.res(X_2, R_2)-R_2, type = "2")
      
      ## 3
      
      frob.CLIME[3,seed] <- norm(clime.res(X_3, R_3)-R_3, type = "F")
      oper.CLIME[3,seed] <- norm(clime.res(X_3, R_3)-R_3, type = "2")
      
      ## 4
      
      frob.CLIME[4,seed] <- norm(clime.res(X_4, solve(R_1))-R_1, type = "F")
      oper.CLIME[4,seed] <- norm(clime.res(X_4, solve(R_1))-R_1, type = "2")
      
      cat(paste0("p = ",p,", seed = ",seed,"\n\n"))
   }
   
   frob.CLIME.mat[ , which(c(30,60) == p)] <- frob.CLIME %>% rowMeans()
   oper.CLIME.mat[ , which(c(30,60) == p)] <- oper.CLIME %>% rowMeans()
}

frob.CLIME.mat
oper.CLIME.mat

################################################ SPICE

compute.spice <- function(X, lambda, q, 
                          tol = 1e-2, 
                          max_iter = 5, max_inner_iter = 5) {
   n <- nrow(X)
   p <- ncol(X)
   Sigma.samp <- cov(X)
   
   d = diag(Sigma.samp)
   Omega.init = diag(1/d)
   T.init = sqrt(Omega.init)
   Omega.prev <- Omega.init
   T.prev <- T.init
   t <- 1
   
   while(t <= max_iter) {
      Omega.curr <- matrix(0, p, p)
      t.dash <- 1
      while(t.dash <= max_inner_iter) {
         T.curr <- matrix(0, p, p)
         for(c in 1:p) {
            for(l in c:p) {
               objective <- function(x) {
                  mat <- matrix(0, p, p)
                  mat[l, c] <- x - T.prev[l, c]
                  T <- T.prev + mat
                  Omega <- t(T) %*% T
                  val <- sum(diag(Omega %*% Sigma.samp)) - log(det(Omega)) + 
                     lambda * (sum(abs(Omega)^q) - sum(diag(abs(Omega)^q)))
                  return(val)
               }
               if(l != c)
                  T.curr[l, c] <- optimise(objective, c(-100, 100))$minimum
               else
                  T.curr[l, c] <- optimise(objective, c(0.01, 100))$minimum
            }
         }
         if(norm((T.curr - T.prev), "F") / norm(T.prev, "F") < tol)
            break
         T.prev <- T.curr
         t.dash <- t.dash + 1
         print(t.dash)
      }
      Omega.curr <- t(T.curr) %*% T.curr
      Omega.curr[abs(Omega.curr) < 1e-5] <- 0
      if(norm(Omega.curr - Omega.prev, "F") / norm(Omega.prev, "F") < tol)
         break
      Omega.prev <- Omega.curr
      t <- t + 1
      print(t)
   }
   return(list(Omega.curr, Sigma.samp))
}

generate.data <- function(n, Sigma) {
   set.seed(123)
   p <- ncol(Sigma)
   X <- MASS::mvrnorm(n, rep(0, p), Sigma)
   return(X)
}

spice.res <- function(p, X, q = 1.5) {
   # Sigma.1 <- matrix(0, p, p)
   # for(i in 1:p) {
   #   for(j in 1:p) {
   #     Sigma.1[i, j] <- 0.7^abs(i-j)
   #   }
   # }
   # X <- generate.data(30, Sigma.1)
   lambda <- 0.5
   Omega <- compute.spice(X, lambda, q)[[1]]
   # accuracy <- sum(diag(Sigma.1 %*% Omega)) - log(det(Sigma.1 %*% Omega)) - nrow(Sigma.1)
   return(Omega)
}

########## SPICE Implementation

frob.SPICE.mat <- matrix(NA, nrow = 4, ncol = 2)
oper.SPICE.mat <- matrix(NA, nrow = 4, ncol = 2)

for(p in c(30,60)){
   
   frob.SPICE <- oper.SPICE <- matrix(NA, nrow = 4, ncol = 3)
   
   for(seed in 1:3){
      R_1 <- P_1(p,seed)
      R_2 <- P_2(p,seed)
      R_3 <- P_3(p,seed)
      
      X_1 <- mvrnorm(n = n, mu = rep(0,p), Sigma = solve(R_1))
      X_2 <- mvrnorm(n = n, mu = rep(0,p), Sigma = solve(R_2))
      X_3 <- mvrnorm(n = n, mu = rep(0,p), Sigma = solve(R_3))
      X_4 <- mvrnorm(n = n, mu = rep(0,p), Sigma = R_1)
      
      
      ## 1
      
      temp <- spice.res(p, X_1)
      
      frob.SPICE[1,seed] <- norm(temp-R_1, type = "F")
      oper.SPICE[1,seed] <- norm(temp-R_1, type = "2")
      # KL.SPICE[1,seed] <- sum(diag(solve(R_1) %*% temp)) - log(det(solve(R_1) %*% temp)) - nrow(solve(R_1))
      
      
      ## 2
      
      temp <- spice.res(p, X_2)
      
      frob.SPICE[2,seed] <- norm(temp-R_2, type = "F")
      oper.SPICE[2,seed] <- norm(temp-R_2, type = "2")
      
      ## 3
      
      temp <- spice.res(p, X_3)
      
      frob.SPICE[3,seed] <- norm(temp-R_3, type = "F")
      oper.SPICE[3,seed] <- norm(temp-R_3, type = "2")
      
      ## 4
      
      temp <- spice.res(p, X_4)
      
      frob.SPICE[4,seed] <- norm(temp-R_1, type = "F")
      oper.SPICE[4,seed] <- norm(temp-R_1, type = "2")
      
      cat(paste0("p = ",p,", seed = ",seed,"\n\n"))
   }
   
   frob.SPICE.mat[ , which(c(30,60) == p)] <- frob.SPICE %>% rowMeans()
   oper.SPICE.mat[ , which(c(30,60) == p)] <- oper.SPICE %>% rowMeans()
}

frob.SPICE.mat
oper.SPICE.mat





