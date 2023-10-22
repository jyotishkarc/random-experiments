
########## Function for performing LMC
lmc.beta <- function(beta.current, X.next, V){
   
   beta.next <- list()
   Y.next <- c()
   
   for (i in 1:N) {
      beta.next[[i]] <- beta.current[[i]]
      
      for(k in 1:K){
         noise <- rnorm(d)
         
         beta.next[[i]] <- beta.next[[i]] - eta * grad.loss(beta.next[[i]], 
                                                            beta.current[[i]], V) +
            sqrt(2*eta*alpha[t]) * noise
      }
      
      Y.next[i] <- X.next[i,] %*% t(beta.next[[i]])
   }
   
   return(Y.next, beta.next)
}

########## Function for computing grad of loss function
grad.loss <- function(beta, beta.hat, V){
   return(2 * V * (beta - beta.hat))
}


########## DRLMC Thompson Sampling
DRLMC.TS <- function(X, v, lambda, gamma, M, alpha, eta, H = 100){
   
   d <- ncol(X[[1]])
   
   Y <- F.val <- W <- V <- beta <- beta.hat <- list()
   m <- c()
   
   F.val[[1]] <- W[[1]] <- beta[[1]] <- beta.hat[[1]] <- m[1] <- 0
   V[[1]] <- diag(lambda, d)
   
   for(t in 2:H){
      res <- list()
      
      for(l in 1:M[t]){
         
         res[[t]] <- lmc.beta(beta[[t-1]], X[[t]], V[[t-1]])
         m[t] <- which.max(res[[t]]$Y.next)
         
         #### compute pi_{m_t}(t)
         
         if(pi.m_t > gamma.param | l == M[t]){
            beta[[t]] <- res[[t]]$beta.next
            break
         }
      }
      
      Y[[t]][m[t]] <- res[[t]]$Y.next[m[t]]
      
      for(i in 1:N){
         Y[[t]][i] <- Y_DR[i] ####
      }
      
      F.val[[t]] <- F.val[[t-1]] + colSums(X[[t]] * Y[[t]])
      W[[t]] <- W[[t-1]] + t(X[[t]]) %*% X[[t]]
      V[[t]] <- W[[t]] + lambda * sqrt(t) * diag(rep(1,ncol(X[[t]])))
      beta.hat[[t]] <- solve(V[[t]]) %*% t(F.val)
      
   }
}










