perm_test_slow <- function(crispr_data, perturbation, M){

  require(tidyverse)

  start.time <- Sys.time()
  pert <- as.character(perturbation)

  df.enh <- crispr_data %>% filter(perturbation == pert)
  df.nt <- crispr_data %>% filter(perturbation == "non-targeting")
  df.combined <- rbind(df.enh,df.nt)

  omega.0 <- (sum(df.enh$HBE1_abs_expr) / sum(df.enh$total_expr)) /
                      (sum(df.nt$HBE1_abs_expr) / sum(df.nt$total_expr))

  m <- nrow(df.enh)
  n <- nrow(df.nt)

  omega <- c()

  for(i in 1:M){
    ifelse(i %% 100 == 0, print(i), TRUE)

    choose.m <- sample.int(m+n, m)

    df.enh <- df.combined %>% slice(choose.m)
    df.nt <- df.combined %>% slice(setdiff(1:(m+n), choose.m))

    omega[i] <- (sum(df.enh$HBE1_abs_expr) / sum(df.enh$total_expr)) /
                      (sum(df.nt$HBE1_abs_expr) / sum(df.nt$total_expr))
  }

  print(Sys.time() - start.time)

  return((1 + sum(omega >= omega.0))/(M+1))
}
