# homework

## 2M1

p_grid <- seq(from = 0, to = 1, length.out = 100)

compute_posterior <- function(w, n, p = p_grid) {
  prior <- rep(x = 1, length((p_grid)))
  likelihood <- dbinom(x = w, size = n, prob = p)
  unstandardized.posterior <- likelihood * prior
  return( unstandardized.posterior / sum(unstandardized.posterior) )
}

plot_posterior <- function(x, y) {
  plot(x = x, y = y, type="b", xlab = "Probability of Water", ylab = "Posterior Probability")
  title <- paste( length(x), "Points")
  mtext(title)
}

# (1)
w <- 3
n <- 3
posterior <- compute_posterior(w = w, n = n)
plot_posterior(x = p_grid, y = posterior)

# (2)
w <- 3
n <- 4
posterior <- compute_posterior(w = w, n = n)
plot_posterior(x = p_grid, y = posterior)

# (3)
w <- 5
n <- 7
posterior <- compute_posterior(w = w, n = n)
plot_posterior(x = p_grid, y = posterior)
