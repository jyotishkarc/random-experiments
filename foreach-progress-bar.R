library(parallel)
library(doSNOW)

numCores<-floor(detectCores() * 0.75)
cl <- makeCluster(numCores)
registerDoSNOW(cl)

# progress bar ------------------------------------------------------------
library(progress)

iterations <- 100                               # used for the foreach loop  

pb <- progress_bar$new(
   format = "letter = :letter [:bar] :elapsed | eta: :eta",
   total = iterations,    # 100 
   width = 60)

progress_letter <- rep(LETTERS[1:10], 10)  # token reported in progress bar

# allowing progress bar to be used in foreach -----------------------------
progress <- function(n){
   pb$tick(tokens = list(letter = progress_letter[n]))
} 

opts <- list(progress = progress)

# foreach loop ------------------------------------------------------------
library(foreach)

foreach(i = 1:iterations, .combine = rbind, .options.snow = opts) %dopar% {
   summary(rnorm(1e6))[3]
}

stopCluster(cl)