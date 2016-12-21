# homework

## 2M1

p_grid <- seq(from = 0, to = 1, length.out = 100)
prior <- rep(x = 1, length = length( p_grid ))

compute_posterior <- function(w, n, prior, p = p_grid) {
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
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)

# (2)
w <- 3
n <- 4
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)

# (3)
w <- 5
n <- 7
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)

## 2M2
prior <- ifelse(test = p_grid < .5, yes = 0, no = 1)

# (1)
w <- 3
n <- 3
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)

# (2)
w <- 3
n <- 4
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)

# (3)
w <- 5
n <- 7
posterior <- compute_posterior(w = w, n = n, prior = prior)
plot_posterior(x = p_grid, y = posterior)

## 2M3
prior <- c(.5, .5)
likelihood <- c(.3, 1)
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
round( posterior[1], 2) == .23

## 2M4
card.1.likelihood <- 2
card.2.likelihood <- 1
card.3.likelihood <- 0
likelihood <- c(card.1.likelihood, card.2.likelihood, card.3.likelihood)
prior <- rep(x = 1, length = 3)
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# the probability the other size is black is equal to the probability that we've drawn card 1
posterior[1] == 2/3
