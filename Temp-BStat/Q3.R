library(magrittr)

theta <- 2
N <- 100
X <- rcauchy(N, theta, 1)

mle.cauchy <- function(X, method = c("NR","fisher"), tol = 1E-6){
   
   differ <- tol + 1
   theta.old.NR <- theta.old.fisher <- mean(X)
   
   print(method)
   
   if("NR" %in% method){
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
            print(theta.new.NR)
            break
         }
         
         theta.old.NR <- theta.new.NR
      }
   }
   
   differ <- tol + 1
   
   if("fisher" %in% method){
      while(differ > tol){
         temp <- X - theta.old.fisher
         
         S <- 2 * sapply(1:N, function(index){
            return(temp[index] / (1 + temp[index]^2))
         }) %>% sum()
         
         theta.new.fisher <- theta.old.fisher + 2*S/N
         differ <- abs(theta.new.fisher - theta.old.fisher)
         
         if(differ <= tol){
            print(theta.new.fisher)
            break
         }
         
         theta.old.fisher <- theta.new.fisher
      }
   }
}





