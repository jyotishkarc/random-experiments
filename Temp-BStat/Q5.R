uniform.EM <- function(data){
   
   order.stats <- sort(X)
   n <- length(data)
   
   consecutive.diff <- order.stats[-1] - order.stats[-n]
   mle <- max(order.stats[which.max(consecutive.diff)], order.stats[n] / 3)
   
   if(max(consecutive.diff) < mle)
      mle <- order.stats[n]
   if(order.stats[1] > max(consecutive.diff))
      mle <- order.stats[n] / 3
   
   return(mle)
}

N <- 100
U <- runif(N)
alpha <- 0.5
X <- as.numeric(N)
theta <- 2

for(i in 1:N){
   if(U[i] <= alpha){
      X[i] <- runif(1,0,theta)
   }else {
      X[i] <- runif(1,2*theta,3*theta)
   }
}
