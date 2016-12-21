# chapter 3

## 3.2

# create posterior
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(x = 1, 1000)
likelihood <- dbinom(x = 6, size = 9, prob = p_grid)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# sample from posterior
samples <- sample(x = p_grid, prob = posterior, size = 1e4, replace = TRUE)

# plot samples sequentially
plot(samples)

# density plot of samples
library(rethinking)
dens(samples)

## 3.6 - intervals of defined boundaries (using just p_grid and posterior)

# what is the probability that p < .5?
sum( posterior[p_grid < .5] )

## 3.7 - intervals of defined boundaries (using samples)

# what is the probability that p < .5?
mean(samples < .5)

# what is the probability .5 < p < .75?
mean(samples > .5 & samples < .75)

## 3.9 - confidence intervals (using samples)

# there is an 80% probability the true parameter is below what value?
quantile(x = samples, probs = .8)

# we are 80% sure the true parameter is between what values?
quantile(x = samples, probs = c(.1, .9))

## 3.11
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(x = 1, 1000)
likelihood <- dbinom(x = 3, size = 3, prob = p_grid)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
samples <- sample(x = p_grid, prob = posterior, size = 1e4, replace = TRUE)

## 3.12 - 50% prediction interval
PI(samples = samples, prob = .5)

## 3.13 - highest posterior density interval
HPDI(samples = samples, prob = .5)

## 3.14 - compute the MAP from the posterior
p_grid[which.max(posterior)]

## 3.15 - compute the MAP from samples of the posterior
chainmode(chain = samples, adj = .01)

## 3.17 - compute weighted average loss, choosing .5 as our point estimate
sum( posterior*abs(.5 - p_grid) )

## 3.18 - compute weighted average loss for all possible point estimates in p_grid
loss <- sapply(X = p_grid, FUN = function(d) sum( posterior*abs(d - p_grid)))

## 3.19 - which point estimate minimizes our loss?
p_grid[which.min(loss)]

## 3.20 - compute binomial density values for land/water example