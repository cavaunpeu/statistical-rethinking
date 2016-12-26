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

# 4.14
mu.list <- seq( from=140, to=160 , length.out=200 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum( dnorm(
  d2$height ,
  mean=post$mu[i] ,
  sd=post$sigma[i] ,
  log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

# 4.15
contour_xyz( post$mu , post$sigma , post$prob )

# 4.16
image_xyz( post$mu , post$sigma , post$prob )

# 4.17
sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]

# 4.18
plot( sample.mu , sample.sigma , cex=0.5 , pch=16 , col=col.alpha(rangi2, 0.1) )

# 4.19
dens(sample.mu)
dens(sample.sigma)

# 4.20
HPDI(sample.mu)
HPDI(sample.sigma)

# 4.24
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

# 4.25
functions.list <- alist(
  height ~ dnorm(mean = mu, sd = sigma),
  mu ~ dnorm(mean = 178, sd = 20),
  sigma ~ dunif(min = 0, max = 50)
)

# 4.26
model <- map(functions.list, data = d2)

# 4.27
precis(model)

# 4.30
vcov(model)

# 4.31
diag( vcov(model) )
cov2cor( vcov(model) )

# 4.32
posterior <- extract.samples(object = model, n = 1e4)
head(posterior)

# 4.33
precis( posterior )
plot(posterior)

# 4.34
library(MASS)
mvrnorm(n = 5, mu = coef(model), Sigma = vcov(model))

# 4.37
plot( d2$height ~ d2$weight )

# 4.38
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

# 4.39
model <- map(
  alist(
    height ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta*weight,
    alpha ~ dnorm(mean = 156, sd = 100),
    beta ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, max = 50)
  ),
  data = d2
)

## 4.40
precis(model)

## 4.41
precis(model, corr = TRUE)

## 4.42 - mean-center the weight
d2$weight.centered <- d2$weight - mean(d2$weight)

## 4.43
model.centered <- map(
  alist(
    height ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta*weight.centered,
    alpha ~ dnorm(mean = 156, sd = 100),
    beta ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, max = 50)
  ),
  data = d2
)

## 4.44
precis(model.centered, corr = TRUE)

## 4.45
plot(height ~ weight, data = d2)
abline( a = coef(model)["alpha"], b = coef(model)["beta"] )

## 4.46
posterior <- extract.samples(model)

## 4.47
posterior[1:5 , ]

## 4.50 - plot the mu that results from many different samples drawn from the posterior, where weight = 50
mu <- posterior$alpha + posterior$beta * 50
dens(mu, col=rangi2, lwd=2, xlab="mu|weight = 50")

## 4.52
HPDI(mu, prob = .89)

## 4.53
mu <- link(model)

## 4.54
weight.seq <- seq(from = 25, to = 70, by = 1)
mu <- link(model, data = data.frame(weight = weight.seq))

## 4.55 - plot the distribution of mu values for pairs of (sample-from-posterior, weight)
plot(height ~ weight, d2, type = "n")
for (i in 1:100)
  points(weight.seq, mu[i,], pch=16, col = col.alpha(rangi2, .1))

## 4.56
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.hpdi <- apply(X = mu, MARGIN = 2, FUN = HPDI, prob = .89)

## 4.57
plot(height ~ weight, data = d2, col = col.alpha(rangi2, .1))
lines(x = weight.seq, y = mu.mean)
shade(object = mu.hpdi, lim = weight.seq)

## 4.59 - simulate heights from our model, not just the mu of the distribution that governs height
sim.height <- sim(model, data = list(weight = weight.seq))

## 4.60 - create a prediction interval
height.PI <- apply(X = sim.height, MARGIN = 2, FUN = PI, prob = .89)

## 4.61 - plot the MAP line, the 89% region of plausible mu, and the boundaries of the simulated heights the model expects

plot(height ~ weight, data = d2, col = col.alpha(rangi2, .25))
lines(x = weight.seq, y = mu.mean)
shade(object = mu.hpdi, lim = weight.seq)
shade(object = height.PI, lim = weight.seq)

## 4.64 - reload Howell data, but this time don't filter for adults
data(Howell1)
d <- Howell1
plot(height ~ weight, data = d)

## 4.65 - standarize weight variable
d$weight.standardized <- (d$weight - mean(d$weight)) / sd(d$weight)

## 4.66 - build polynomial regression model
d$weight.standardized.squared <- d$weight.standardized^2
model.poly <- map(
  alist(
    height ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta.1*weight.standardized + beta.2*weight.standardized.squared,
    alpha ~ dnorm(mean = 140, sd = 100),
    beta.1 ~ dnorm(mean = 0, sd = 10),
    beta.2 ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, max = 50)
  ),
  data = d
)

## 4.68 - summarize results
weight.seq <- seq(from = -2.2, to = 2, length.out = 30)
prediction.data <- list(weight.standardized = weight.seq, weight.standardized.squared = weight.seq^2)
mu <- link(model.poly, data = prediction.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI)
height.simulations <- sim(model.poly, data = prediction.data)
height.simulations.PI <- apply(X = height.simulations, MARGIN = 2, FUN = PI, prob = .89)

## 4.69 - plot result summaries
plot(height ~ weight.standardized, data = d, col = col.alpha(rangi2, .5))
lines(x = weight.seq, y = mu.mean)
shade(object = mu.PI, lim = weight.seq)
shade(object = height.simulations.PI, lim = weight.seq)

## 4.70 - fit a cubic regression

# build cubic regression model
d$weight.standardized.cubed <- d$weight.standardized^3
model.poly <- map(
  alist(
    height ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta.1*weight.standardized + beta.2*weight.standardized.squared + beta.3*weight.standardized.cubed,
    alpha ~ dnorm(mean = 178, sd = 100),
    beta.1 ~ dnorm(mean = 0, sd = 10),
    beta.2 ~ dnorm(mean = 0, sd = 10),
    beta.3 ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, max = 50)
  ),
  data = d
)

# summarize results
weight.seq <- seq(from = -2.2, to = 2, length.out = 30)
prediction.data <- list(weight.standardized = weight.seq, weight.standardized.squared = weight.seq^2, weight.standardized.cubed = weight.seq^3)
mu <- link(model.poly, data = prediction.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI)
height.simulations <- sim(model.poly, data = prediction.data)
height.simulations.PI <- apply(X = height.simulations, MARGIN = 2, FUN = PI, prob = .89)

# plot result summaries
plot(height ~ weight.standardized, data = d, col = col.alpha(rangi2, .5))
lines(x = weight.seq, y = mu.mean)
shade(object = mu.PI, lim = weight.seq)
shade(object = height.simulations.PI, lim = weight.seq)