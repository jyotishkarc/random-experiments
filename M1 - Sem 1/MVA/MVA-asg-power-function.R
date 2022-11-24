## Author: JYOTISHKA RAY CHOUDHURY
## Roll No.: MB2203

library(dplyr)
library(partitions)
library(reshape2)
library(ggplot2)

lambda.seq <- c(1,0,-0.5,-1,-2)
delta.seq <- seq(-1/4,1/12, by = 0.001) %>% setdiff(c(-1/4,1/12))

df <- t(compositions(20,4,T))
df.new <- df %>% cbind(1:nrow(df)) %>% as.data.frame()
colnames(df.new) <- c("V1","V2","V3","V4","ind")


I <- function(lambda, x){
   
   if(lambda == 1){
      pds <- sum(x * (x - 5)) / 5
      return(pds)
   }
   
   if(lambda == 0){
      # x <- x[-which(x == 0)]
      pds <- 2 * sum(x * log(x/5))
      return(pds)
   }
   
   if(lambda == -0.5){
      pds <- 160 - 8*sqrt(5)*sum(sqrt(x))
      return(pds)
   }
   
   if(lambda == -1){
      pds <- 10 * sum(log(5/x))
      return(pds)
   }
   
   if(lambda == -2){
      pds <- 25 * sum(1/sqrt(x)) - 20
      return(pds)
   }
}


cum.func <- function(lambda, mat){
   
   I.all <- mat[,-5] %>% apply(1, function(val) return(I(lambda, val)))
   
   mat.prob <- mat[,-5] %>% as.matrix() %>% 
                  apply(1, function(val) return(dmultinom(x = val, 
                                                          prob = rep(0.25,4))))

   mat.sort <- mat %>% cbind(I.all, mat.prob) %>% arrange(I.all)
   mat.sort.prob <- mat.sort %>% cbind(cum.prob = cumsum(.$mat.prob))
   
   return(mat.sort.prob)
}


res <- power.H1 <- list()
alpha <- 0.95

for(k in 1:length(lambda.seq)){
   start.time <- proc.time()[3]
   res[[k]] <- cum.func(lambda.seq[k], df.new)
   
   cutoff.ind <- max(which(res[[k]]$cum.prob <= alpha))
   gamma.rand <- (res[[k]]$cum.prob[cutoff.ind+1] - alpha) / 
                     (res[[k]]$cum.prob[cutoff.ind+1] - res[[k]]$cum.prob[cutoff.ind])
   
   power.H1[[k]] <- 0
   
   for(i in 1:length(delta.seq)){
      
      prob.H1 <- c(rep(0.25+delta.seq[i], 3), 0.25-3*delta.seq[i])
      
      temp <- res[[k]][ ,1:4] %>% as.matrix() %>% 
                     apply(1, function(val){
                        return(dmultinom(x = val, prob = prob.H1))})
      
      power.H1[[k]][i] <- sum(temp[(cutoff.ind+2) : length(temp)]) + 
                                    gamma.rand * temp[cutoff.ind+1]
   }
   
   print(proc.time()[3] - start.time)
   print(k)
}


power.fn <- data.frame(x = delta.seq,
                       "lambda = 1" = power.H1[[1]],
                       "lambda = 0" = power.H1[[2]],
                       "lambda = -0.5" = power.H1[[3]],
                       "lambda = -1" = power.H1[[4]],
                       "lambda = -2" = power.H1[[5]])

data.melt <- melt(power.fn, id="x")

ggplot(data = data.melt, aes(x=x, y=value, colour=variable)) + 
   geom_line(size = 1.3) +
   scale_color_manual(labels = c("lambda = 1", "lambda = 0", "lambda = -0.5",
                                 "lambda = -1", "lambda = -2"),
                      values = c("1", "2","3","4","6")) +
   theme(legend.position="bottom") +
   xlab("delta") + ylab("Power")










