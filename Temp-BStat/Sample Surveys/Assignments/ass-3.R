
sub.strata <- fsu <- household <- list()

S <- sapply(1:nrow(TUS106_L01), function(val){
   return(substr(TUS106_L01[val,1],22,23))
})

strata <- labels(table(S))$S
# strata <- unique(S)

for(s in 1:length(strata)){
   pos.s <- which(S == strata[s])
   print(s)
   sub.strata[[s]] <- sapply(TUS106_L01$V1[pos.s], function(val){
      return(substr(val,24,25))
   })
   
   no.substratum.s <- labels(table(sub.strata[[s]]))[[1]]
   print(no.substratum.s)
   
   fsu[[s]] <- list()

   for(t in 1:length(no.substratum.s)){
      pos.st <- which(sub.strata[[s]] == no.substratum.s[t])

      fsu[[s]][[t]] <- sapply(TUS106_L01$V1[pos.st], function(val){
         return(substr(val,4,8))
      })
      
      no.household.s.t <- labels(table(fsu[[s]][[t]]))[[1]]
      print(no.household.s.t)
      print("Hello")
      # household[[s]][[t]] <- list()
      
      # for(h in 1:length(no.household.s.t)){
      #    pos.st.h <- which(fsu[[s]][[t]] == no.household.s.t[h])
      # 
      #    household[[s]][[t]][[h]] <- sapply(TUS106_L01$V1[pos.st.h], function(val){
      #       return(substr(val,31,32))
      #    })
      # }

      print("Hello Hello")
      
   }
   
   
   
}




