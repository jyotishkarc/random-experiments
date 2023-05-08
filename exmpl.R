
args <- commandArgs(trailingOnly = TRUE)
print(args)

library(magrittr)

a <- args[1] %>% as.numeric()
b <- args[2] %>% as.numeric()
c <- args[3] %>% as.numeric()

rm(args)

f <- function(a,b,c){
   return(a+b*c)
}

f(a,b,c)







# f(a^2,b^2,c^2)


if(FALSE){
   options(echo=TRUE) # if you want see commands in output file
   args <- commandArgs(trailingOnly = TRUE)
   print(args)
   # trailingOnly=TRUE means that only your arguments are returned, check:
   # print(commandArgs(trailingOnly=FALSE))
   
   start_date <- as.Date(args[1])
   name <- args[2]
   n <- as.integer(args[3])
   rm(args)
   
   # Some computations:
   x <- rnorm(n)
   png(paste(name,".png",sep=""))
   plot(start_date+(1L:n), x)
   dev.off()
   
   summary(x)
}