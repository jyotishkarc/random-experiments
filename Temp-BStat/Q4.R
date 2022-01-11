library(magrittr)

blood.EM <- function(N,p) {
   
   #E-step
   N.aa <- N[1] * p[1]^2 / (p[1]^2 + 2*p[1]*p[3])
   N.ao <- N[1] * 2 * p[1] * p[3] / (p[1]^2 + 2*p[1]*p[3])
   N.bb <- N[2] * p[2]^2 / (p[2]^2 + 2*p[2]*p[3])
   N.bo <- N[2] * 2 * p[2] * p[3] / (p[2]^2 + 2*p[2]*p[3])
   
   #M-step
   n <- sum(N)
   
   p[1] <- (2 * N.aa + N.ao + N[3]) / (2 * n)
   p[2] <- (2 * N.bb + N.bo + N[3]) / (2 * n)
   p[3] <- 1 - p[1] - p[2]
   #p[3] <- (2 * N.ao + N.bo + N[4]) / (2 * n)
   
   return(p)
}

blood.fisher <- function(N, parameters){
   N.A <- N[1]
   N.B <- N[2]
   N.AB <- N[3]
   N.O <- N[4]
   n <- sum(N)
   
   p <- parameters[1]
   q <- parameters[2]
   r <- parameters[3]
   
   V <- matrix(0, nrow = 2, ncol = 1, byrow = TRUE)
   V[1,1] <- N.A*(1/p-1/(p+2*r))-2*N.B/(q+2*r)+N.AB/p-(2*N.O/r)
   V[2,1] <- N.B*(1/q-1/(q+2*r))-2*N.A/(p+2*r)+N.AB/q-(2*N.O/r)
   
   H <- matrix(0,nrow = 2, ncol = 2, byrow = TRUE)
   
   H[1,1] <- -((n*(p+2*r))/p + (n*p)/(p+2*r) + (4*n*q)/(q+2*r) + (2*n*q)/p + (2*n))
   H[1,2] <- H[2,1] <- -((2*n*p)/(p+2*r) + (2*n*q)/(q+2*r) + (2*n))
   # H[2,1] <- -((2*n*p)/(p+2*r) + (2*n*q)/(q+2*r) + (2*n))
   H[2,2] <- -((n*(q+2*r))/q + (n*q)/(q+2*r) + (4*n*p)/(p+2*r) + (2*n*p)/q + (2*n))
   
   vec <- matrix(c(p,q), nrow=2, ncol=1, byrow=TRUE)
   arr <- vec - (solve(H)) %*% V
      
   return(c(arr,1-sum(arr)))
}


N <- c(292,323,122,263)
tol <- 1E-6

p <- c(1,1,1)/3
p.old.em <- p
p.new.em <- p + 5

while(TRUE){
   p.new.em <- blood.EM(N, p.old.em)
   
   if(as.numeric((p.new.em - p.old.em) %*% (p.new.em - p.old.em)) <= tol){
      break
   }
   
   p.old.em <- p.new.em
   print(p.old.em)
}

print(p.new.em)
sum(p.new.em) %>% print()


p <- c(1,1,1)/3
p.old.f <- p
p.new.f <- p + 5

while(TRUE){
   p.new.f <- blood.fisher(N, p.old.f)
   
   if(as.numeric((p.new.f - p.old.f) %*% (p.new.f - p.old.f)) <= tol){
      break
   }
   
   p.old.f <- p.new.f
   print(p.old.f)
}

print(p.new.f)
sum(p.new.f) %>% print()







