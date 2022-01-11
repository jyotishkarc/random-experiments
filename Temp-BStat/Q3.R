library(magrittr)

theta <- 3
N <- 100

mle.cauchy.NR <- function(X, tol = 1E-6){
   
   differ <- tol + 1
   theta.old.NR <- median(X)
   
   while(differ > tol){
      temp <- X - theta.old.NR
      
      S <- 2 * sapply(1:N, function(index){
         return(temp[index] / (1 + temp[index]^2))
      }) %>% sum()
      
      H <- 2 * sapply(1:N, function(index){
         return((temp[index]^2 - 1) / (1 + temp[index]^2)^2)
      }) %>% sum()
      
      theta.new.NR <- theta.old.NR - S/H
      differ <- abs(theta.new.NR - theta.old.NR)
      
      if(differ <= tol){
         cat("NR gives",theta.new.NR,"\n")
         break
      }
      
      theta.old.NR <- theta.new.NR
   }
   return(theta.new.NR)
}


mle.cauchy.fisher <- function(X, tol = 1E-6){
   
   differ <- tol + 1
   theta.old.fisher <- median(X)
   
   while(differ > tol){
      temp <- X - theta.old.fisher
      
      S <- 2 * sapply(1:N, function(index){
         return(temp[index] / (1 + temp[index]^2))
      }) %>% sum()
      
      theta.new.fisher <- theta.old.fisher + 2*S/N
      differ <- abs(theta.new.fisher - theta.old.fisher)
      
      if(differ <= tol){
         cat("Fisher's Scoring gives",theta.new.fisher,"\n")
         break
      }
      theta.old.fisher <- theta.new.fisher
   }
   
   time.fisher <- proc.time()[3] - t1.fisher
   return(theta.new.fisher)
}

X <- list()
res.NR <- res.fisher <- c()
iter <- 1000 * 5

for(i in 1:iter){
   X[[i]] <- rcauchy(N, theta, 1)
}

t1.NR <- proc.time()[3]
for(i in 1:iter){
   res.NR[i] <- mle.cauchy.NR(X[[i]])
}
time.NR <- proc.time()[3] - t1.NR

t1.fisher <- proc.time()[3]
for(i in 1:iter){
   res.fisher[i] <- mle.cauchy.fisher(X[[i]])
}
time.fisher <- proc.time()[3] - t1.fisher














