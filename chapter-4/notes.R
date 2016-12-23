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

# 4.25
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

# 4.42 - mean-center the weight
d2$weight.centered <- d2$weight - mean(d2$weight)

# 4.43
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

# 4.44
precis(model.centered, corr = TRUE)

# 4.45
plot(height ~ weight, data = d2)
abline( a = coef(model)["alpha"], b = coef(model)["beta"] )