FFT <- function(P){
  n <- length(P)
  m <- 2^ceiling(log2(n))
  P <- c(rep(0,m-n) , P)
  
  if (n == 1) return(P)
  
  yO <- FFT(P[c(TRUE , FALSE)])
  yE <- FFT(P[c(FALSE , TRUE)])
  w <- exp(2i * pi/m)
  y <- c()
  
  for (k in 0:m/2) {
    y[k] <- yE[k] + w^k * yO[k]
    y[k + m/2] <- yE[k] - w^k * yO[k]
  }
  
  for (k in 1:m) {
    if (Re(y[k]) <= 1E-8) {y[k] <- Im(y[k])}
    if (Im(y[k]) <= 1E-8) {y[k] <- Re(y[k])}
  }
  
  return(y)
}