# notes

library(rethinking)

## 4.1 - simulate random walks
final.positions <- replicate(n = 1000, sum(runif(n = 16, min = -1, max = 1)))

## 4.3 - simulate multiplicative growth rates
growth <- replicate(n = 1e5, expr = prod(1 + runif(n = 12, min = 0, max = .01)))
dens(growth, norm.comp = TRUE)

## 4.4 - simulate multiplicative growth rates for big and small individual percetile growth ranges
growth.small <- replicate(n = 1e5, expr = prod(1 + runif(n = 12, min = 0, max = .01)))
growth.big <- replicate(n = 1e5, expr = prod(1 + runif(n = 12, min = 0, max = .5)))

## 4.5 - simulate log-multiplicative growth rates
growth <- replicate(n = 1e5, expr = log( prod(1 + runif(n = 12, min = 0, max = .01)) ))
dens(growth, norm.comp = TRUE)

## 4.7
data(Howell1)
d <- Howell1

## 4.10
d2 <- d[ d$age >= 18 , ]

## 4.11 - plot prior for mu
mu.prior.mu <- 178
mu.prior.sigma <- 20
curve(expr = dnorm(x = x, mean = mu.prior.mu, sd = mu.prior.sigma), from = 100, to = 250)

## 4.12 - plot prior for sigma
sigma.prior.lower.bound <- 0
sigma.prior.upper.bound <- 50
curve(expr = dunif(x = x, min = sigma.prior.lower.bound, max = sigma.prior.upper.bound), from = -10, to = 60)

# 4.13 - plot our composite prior itself
trials <- 1e4
samples.prior.mu <- rnorm(n = trials, mean = mu.prior.mu, sd = mu.prior.sigma)
samples.prior.sd <- runif(n = trials, min = sigma.prior.lower.bound, max = sigma.prior.upper.bound)
samples.prior.height <- rnorm(n = trials, mean = samples.prior.mu, sd = samples.prior.sd)
dens(samples.prior.height)
