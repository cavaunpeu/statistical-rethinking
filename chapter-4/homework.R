# homework

library(rethinking)

## 4M1
trials <- 1e3
mu.prior.samples <- rnorm(n = trials, mean = 0, sd = 10)
sigma.prior.samples <- runif(n = trials, min = 0, max = 10)
simulated.heights.from.prior <- rnorm(n = trials, mean = mu.prior.samples, sd = sigma.prior.samples)

## 4M2
formula.list <- alist(
  y ~ dnorm( mu , sigma ),
  mu ~ dnorm( 0 , 10 ),
  sigma ~ dunif( 0 , 10 )
)

## 4H1

# load data
data(Howell1)
d <- Howell1
d$weight.centered <- (d$weight - mean(d$weight)) / sd(d$weight)

# build model
model <- map(
  alist(
    height ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta*weight.centered,
    alpha ~ dnorm(mean = 0, sd = 100),
    beta ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, max = 64)
  ),
  data = d
)

# simulate heights from model
individual.weights <- c(46.95, 43.72, 64.78, 32.59, 54.63)
individual.weights.centered <- (individual.weights - mean(d$weight)) / sd(d$weight)
simulated.heights <- sim(model, data = list(weight.centered = individual.weights.centered))

# summarize results
simulated.heights.mean <- apply(X = simulated.heights, MARGIN = 2, FUN = mean)
simulated.heights.PI <- apply(X = simulated.heights, MARGIN = 2, FUN = PI, prob = .89)

# try it manually for the first individual
posterior.samples <- extract.samples(model)
simulated.heights.first.individual <- rnorm(n = trials, mean = posterior.samples$alpha + posterior.samples$beta*individual.weights.centered[1], sd = posterior.samples$sigma)
simulated.heights.first.individual.mean <- mean(simulated.heights.first.individual)
simulated.heights.first.individual.PI <- PI(samples = simulated.heights.first.individual, prob = .89)

## 4H2
data(Howell1)
d <- Howell1
d <- d[d$age < 18,]

# scale weight column
d$weight.standardized <- d$weight

# a)
model <- map(
  alist(
    height ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta*weight.standardized,
    alpha ~ dnorm(mean = 100, sd = 100),
    beta ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, max = 50)
  ),
  data = d
)

precis(model)

# b)
library(MASS)
trials <- 1e5

weight.seq <- seq(from = 1, to = 45, length.out = 50)

# simulate mu then compute mean and hpdi
posterior.samples <- data.frame( mvrnorm(n = trials, mu = coef(model), Sigma = vcov(model)) )
mu.link <- function(weight) posterior.samples$alpha + posterior.samples$beta * weight
mu <- sapply(X = weight.seq, FUN = mu.link)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.hpdi <- apply(X = mu, MARGIN = 2, FUN = HPDI, prob = .89)

# simulate heights then compute hpdi
height.link <- function(weight) rnorm(n = nrow(posterior.samples), mean = mu.link(weight), sd = posterior.samples$sigma)
height.samples <- sapply(X = weight.seq, FUN = height.link)
height.hpdi <- apply(X = height.samples, MARGIN = 2, FUN = HPDI, prob = .89)

# plot results
plot(height ~ weight.standardized, data = d, col = col.alpha(rangi2, .5))
lines(x = weight.seq, y = mu.mean)
shade(object = mu.hpdi, lim = weight.seq)
shade(object = height.hpdi, lim = weight.seq)
