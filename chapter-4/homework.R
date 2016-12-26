# homework

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