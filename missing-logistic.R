library(dplyr)
library(ggplot2)
library(pbapply)
library(tictoc)

set.seed(2203)

simulate_error <- function(n, d = 100, missing_prob = 0.3) {
   theta0 <- rnorm(d)
   theta0 <- theta0 / sqrt(sum(theta0^2))
   
   X <- matrix(rnorm(n * d), nrow = n, ncol = d)
   logits <- X %*% theta0
   probs <- 1 / (1 + exp(-logits))  # P(y = -1)
   y <- ifelse(runif(n) < probs, -1, 1)
   
   W <- matrix(rbinom(n * d, 1, 1 - missing_prob), nrow = n, ncol = d)
   Z <- X * W
   
   n_j <- colSums(W)
   n_jk <- t(W) %*% W
   
   mu_hat <- sapply(1:d, function(j) {
      denom <- max(1, n_j[j])
      sum(y * Z[, j]) / denom
   })
   
   Sigma_hat <- matrix(0, d, d)
   for (j in 1:d) {
      for (k in 1:d) {
         if (j == k) {
            denom <- max(1, n_j[j])
            Sigma_hat[j, k] <- sum(Z[, j]^2) / denom
         } else {
            denom <- max(1, n_jk[j,k])
            Sigma_hat[j, k] <- sum(Z[, j] * Z[, k]) / denom
         }
      }
   }
   
   # Project onto PSD cone: symmetrize and truncate negative eigenvalues
   eig <- eigen((Sigma_hat + t(Sigma_hat)) / 2, symmetric = TRUE)
   Sigma_proj <- eig$vectors %*% diag(pmax(eig$values, 0)) %*% t(eig$vectors)
   
   # Sigma_proj <- Sigma_hat
   
   # Solve for theta_hat
   theta_hat <- tryCatch(
      solve(Sigma_proj, mu_hat),
      error = function(e) MASS::ginv(Sigma_proj) %*% mu_hat
   )
   
   return(list(y = y, X = X, W = W, Z = Z,
               error = sqrt(sum((theta_hat - theta0)^2))))
}

{
   tictoc::tic()
   
   d <- 100
   # ns <- c(200, 400, 800, 1600, 3200, 6400)
   ns <- seq.int(400,20000, by = 400)
   res <- pbapply::pblapply(ns, simulate_error)
   errors <- res %>% lapply(function(lst) lst$error) %>% unlist()
   
   tictoc::toc()  
}


df <- data.frame(
   n = ns,
   error_norm = errors,
   sqrt_d_over_n = sqrt(d / ns),
   exp_1_by_d = ns^(-1/d)
)

df_long <- df %>%
   tidyr::pivot_longer(cols = c(error_norm, sqrt_d_over_n),
                names_to = "quantity",
                values_to = "value")

# breaks_to_show <- ns[c(1:3,5, seq.int(7, length(ns), by = 14))]
# labels_to_show <- which(diff(log10(df$n)) >= 0.045)

df_long |> 
   ggplot(aes(x = n, y = value, color = quantity)) +
      geom_line(linewidth = 1) +
      geom_smooth(linewidth = 0.7, method = 'loess', se = F, linetype = 'dotted') +
      geom_point(size = 1) +
      scale_color_manual(
         values = c("error_norm" = "steelblue", 
                    "sqrt_d_over_n" = "orange"),
                    # "exp_1_by_d" = "red"),
         labels = c(
            error_norm = bquote('||' * hat(theta)[n] - theta[0] * '||'[2]),
            sqrt_d_over_n = bquote(sqrt(d/n)))
            # exp_1_by_d = bquote(n^{-1/d})) +
         ) +
      # scale_x_log10(breaks = breaks_to_show, labels = breaks_to_show) +
      labs(
         # x = "Sample size (n) in log scale",
         x = "Sample size (n)",
         y = "Value",
         title = "Behavior of OLS estimator error norm for logistic regression",
         color = ""
      ) +
      theme_linedraw() +
      theme(
         text = element_text(size = 13),
         legend.position = "bottom",
         plot.title.position = "plot",  # aligns relative to the full plot
         plot.title = element_text(face = 'bold', hjust = 0.5),  # centers the title
         axis.title.x = element_text(margin = margin(t = 10))
      )
   

   
   
   
   
   
   
   
   
   
   
   
   
   
