
########## Function for Six-Point Lagrangian Interpolation
inter <- function(target, x, vec){
   S <- 0
   num <- length(x)
   
   if(target == 2){
      x <- c(100,1,5,10,15,20)
   }
   
   if(target == 3){
      x <- c(100,1,2,5,10,15)
   }
   
   for(i in 1:num){
      denom <- x[setdiff(1:6, i)] 
      S <- S + prod(target - denom) * vec[x[i]] / prod(x[i] - denom)
   }
   
   return(S)
}


########## Index of Neighbours of Target
neighbours <- function(target, len, vec){
   
   temp <- which(is.na(vec) == FALSE)
   
   left <- temp[temp < target]
   right <- temp[temp > target]
   
   if(length(left) < 3){
      index <- c(sort(left), sort(right)[1:(len - length(left))])
   }
   else if(length(right) < 3){
      index <- c(sort(left, decreasing = TRUE)[1:(len - length(right))], sort(right))
   }
   else index <- c(sort(sort(left, decreasing = TRUE)[1:3]),
                   sort(right)[1:3])
   
   return(index)
}


#################### Function for Complete Life Table
complete.LT <- function(abridged.LT){
   
   abridged.LT <- abridged.LT[-c(20,40),]
   complete <- list()
   
   for(i in 1:3){
      complete[[i]] <- list()
      
      for(j in 1:3){
         l <- rep(NA, 100)
         K <- abridged.LT[(19*(i-1) + 2):(19*i) , (4*j - 1)]
         l <- replace(l, c(1,seq(5, 85, by = 5)), K)
         l[100] <- 100000
         
         for(k in 2:85){
            if(is.na(l[k]) == FALSE) next
            l[k] <- inter(k, neighbours(k, 6, l), l)
         }
         
         q <- L <- e <- c()
         
         for(x in 2:84){
            q[x] <- 1 - l[x+1]/l[x]
            L[x] <- mean(c(l[x],l[x+1]))
         }
         
         q[1] <- abridged.LT[(19*(i-1)+2),(4*j-2)]
         q[85] <- 0
         
         L[1] <- abridged.LT[(19*(i-1)+2),(4*j)]
         L[85] <- abridged.LT[(19*i),(4*j)]
         
         for(x in 2:84){
            e[x] <- sum(L[x:85]) / l[x]
         }
         
         e[1] <- abridged.LT[(19*(i-1)+2),(4*j+1)]
         e[85] <- abridged.LT[(19*i),(4*j+1)]
         
         complete[[i]][[j]] <- data.frame(#"Age" = 0:85,
            "q_x" = sapply(c(abridged.LT[(19*(i-1)+1),(4*j-2)],q), function(val){
                                    return(val %>% as.numeric() %>% round(4))}),
            "l_x" = sapply(c(100000,l[1:85]), function(val){
                                    return(val %>% as.numeric() %>% ceiling())}), 
            "L_x" = sapply(c(abridged.LT[(19*(i-1)+1),(4*j)],L), function(val){
                                    return(val %>% as.numeric() %>% ceiling())}),
            "e_x" = sapply(c(abridged.LT[(19*(i-1)+1),(4*j+1)],e), function(val){
                                    return(val %>% as.numeric() %>% round(1))}))
         
         rownames(complete[[i]][[j]]) <- 0:85
      }
   }
   
   return(complete)
}














