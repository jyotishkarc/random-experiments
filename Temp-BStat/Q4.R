#EM iteration
# Arguments:
# N=(Na,Nb,Nab,No)
# p=(pa,pb,po)
emstep <- function(N,p) {
   
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

N <- c(292,323,122,263)
p <- c(1,1,1)/3
p.old <- p
p.new <- p + 5
tol <- 1E-8

while(as.numeric((p.new - p.old) %*% (p.new - p.old)) > tol){
   p.new <- emstep(N, p.old)
   
   if(as.numeric((p.new - p.old) %*% (p.new - p.old)) <= tol){
      break
   }
   
   p.old <- p.new
}

print(p.new)
sum(p.new) %>% print()
