# homework

## 2M1

p_grid <- seq(from = 0, to = 1, length.out = 100)
prior <- rep(x = 1, length = length(p_grid))

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
prior <- rep(x = 1, length = length(likelihood))
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# the probability the other size is black is equal to the probability that we've drawn card 1
posterior[1] == 2/3

## 2M5
card.1.likelihood <- 2
card.2.likelihood <- 1
card.3.likelihood <- 0
card.4.likelihood <- 2
likelihood <- c(card.1.likelihood, card.2.likelihood, card.3.likelihood, card.4.likelihood)
prior <- rep(x = 1, length = length(likelihood))
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# the probability the other size is black is equal to the probability that we've drawn card 1 or 4
posterior[1] + posterior[4]

## 2M6
card.1.likelihood <- 2
card.2.likelihood <- 1
card.3.likelihood <- 0
likelihood <- c(card.1.likelihood, card.2.likelihood, card.3.likelihood)
prior <- c(1, 2, 3)
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# the probability the other size is black is equal to the probability that we've drawn card 1
posterior[1] == .5

## 2M7
card.1.likelihood <- 2
card.2.likelihood <- 1
card.3.likelihood <- 0
likelihood <- c(card.1.likelihood, card.2.likelihood, card.3.likelihood)
prior <- c(1, 2, 3)
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# the probability the other size is black is equal to the probability that we've drawn card 1
posterior[1] == .5

## 2M8
card.1.2.likelihood <- 2
card.2.1.likelihood <- 0
card.1.3.likelihood <- 4
card.3.1.likelihood <- 0
card.2.3.likelihood <- 2
card.3.2.likelihood <- 0

likelihood <- c(card.1.2.likelihood, card.2.1.likelihood, card.1.3.likelihood, card.3.1.likelihood, card.2.3.likelihood, card.3.2.likelihood)
prior <- rep(x = 1, length = length(likelihood))
unstandardized.posterior <- prior * likelihood
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# the probability that the other side of the first card is black is equal to the probability that the first card is card 1,
# which equals the probability that the sequence we've chosen is either (1, 2), or (1, 3)
posterior[1] + posterior[3] == .75

## 2H1

# find posterior for plausibility of each pandas species following the first birth of twins
species.1.likelihood <- .1
species.2.likelihood <- .2
likelihood <- c(species.1.likelihood, species.2.likelihood)
prior <- c(1, 1)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# probability next birth is set of twins
posterior[1] * .1 + posterior[2] * .2

## 2H2
species.1.likelihood <- .1
species.2.likelihood <- .2
likelihood <- c(species.1.likelihood, species.2.likelihood)
prior <- c(1, 1)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# probability pandas is from species 1
posterior[1]