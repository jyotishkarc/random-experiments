# Clear environment
rm(list = ls())

library(ggplot2)

# Parameters
n <- 5000
p <- 10
dim <- p * (p + 1) / 2

# --- Helper Functions ---

upperTriIndexToIJ <- function(iPrime, n) {
   if (iPrime < 1 || iPrime > n * (n + 1) / 2 || iPrime != floor(iPrime)) {
      stop("iPrime must be an integer between 1 and n*(n+1)/2.")
   }
   i <- ceiling(((2 * n + 1) - sqrt((2 * n + 1)^2 - 8 * iPrime)) / 2)
   C_prev <- (i - 1) * (2 * n - i + 2) / 2
   j <- i + (iPrime - C_prev) - 1
   return(c(i, j))
}

buildOperatorMatrix <- function(x, p) {
   n <- ncol(x)
   dim <- p * (p + 1) / 2
   
   Q <- matrix(0, nrow = dim, ncol = n)
   
   for (k in 1:dim) {
      idx <- upperTriIndexToIJ(k, p)
      a <- idx[1]
      b <- idx[2]
      if (a == b) {
         Q[k, ] <- x[a, ]^2
      } else {
         Q[k, ] <- sqrt(2) * x[a, ] * x[b, ]
      }
   }
   
   M <- (Q %*% t(Q)) / n
   M <- (M + t(M)) / 2
   return(M)
}

# --- Case 1: Standard Gaussian ---

set.seed(42)
x <- matrix(rnorm(p * n), nrow = p, ncol = n)
M <- buildOperatorMatrix(x, p)
evals1 <- sort(eigen((M + t(M)) / 2, symmetric = TRUE)$values, decreasing = TRUE)

df1 <- data.frame(
   index = seq_along(evals1),
   eigenvalue = evals1
)

ggplot(df1, aes(x = index, y = eigenvalue)) +
   geom_segment(aes(xend = index, yend = 0), color = "steelblue") +
   geom_point(color = "steelblue", size = 1.5) +
   labs(
      title = paste0("Eigenvalues for 1 Gaussian, n = ", n, ", p = ", p),
      x = "Index",
      y = "Eigenvalue"
   ) +
   theme_bw()

# --- Case 2: Symmetric Two-Component Mean Mixture ---

a_val <- 4
mu <- rep(0, p)
mu[1] <- a_val

s <- 2 * (runif(n) < 0.5) - 1
x <- matrix(rnorm(p * n), nrow = p, ncol = n) + mu %o% s

M <- buildOperatorMatrix(x, p)
evals2 <- sort(eigen((M + t(M)) / 2, symmetric = TRUE)$values, decreasing = TRUE)

cat("Top 15 eigenvalues:\n")
print(evals2[1:15])

df2 <- data.frame(
   index = seq_along(evals2),
   eigenvalue = evals2
)

ggplot(df2, aes(x = index, y = eigenvalue)) +
   geom_segment(aes(xend = index, yend = 0), color = "tomato") +
   geom_point(color = "tomato", size = 1.5) +
   labs(
      title = paste0("Eigenvalues for 2 mixture, n = ", n, ", p = ", p),
      x = "Index",
      y = "Eigenvalue"
   ) +
   theme_bw()

