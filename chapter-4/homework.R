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

