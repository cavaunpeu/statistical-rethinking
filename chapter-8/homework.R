# homework

## 8M1

library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]
dd.trim <- dd[ , c("log_gdp", "rugged", "cont_africa")]

# put a uniform prior on sigma
m8.1.unif <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data=dd.trim )

# put an exponential prior on sigma
m8.1.exp <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ), data=dd.trim )

# visualize each prior
curve(dcauchy(x, 0, 2), from = 0, to = 10, xlab = "sigma", ylab = "Density", ylim = c(0, 1))
curve(dunif(x, 0, 10), from = 0, to = 10, add = TRUE, col = "blue")
curve(dexp(x, 1), from = 0, to = 10, add = TRUE, col = "red")

# plot the posterior for sigma for each model
sigma_unif <- extract.samples(m8.1.unif,pars="sigma")
sigma_exp <- extract.samples(m8.1.exp,pars="sigma")
dens(sigma_unif[[1]], xlab="sigma", xlim=c(0.5,1.5), col="red")
dens(sigma_exp[[1]], add=TRUE, col="blue")

## 8M2

# fit models with a cauchy prior on sigma for varying scale parameter values 
m8.2.cauchy.10 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 10)
  ), data=dd.trim )

m8.2.cauchy.1 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 1)
  ), data=dd.trim )

m8.2.cauchy.point.1 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dcauchy(0, .1)
  ), data=dd.trim )


# fit models with an exponential prior on sigma for varying scale parameter values 
m8.2.exp.10 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dexp(10)
  ), data=dd.trim )

m8.2.exp.1 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dexp(1)
  ), data=dd.trim )

m8.2.exp.point.1 <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0, 100),
    bR ~ dnorm(0, 10),
    bA ~ dnorm(0, 10),
    bAR ~ dnorm(0, 10),
    sigma ~ dexp(.1)
  ), data=dd.trim )

# plot the posterior distribution for sigma under the cauchy priors
sigma.cauchy.10 <- extract.samples(m8.2.cauchy.10, pars="sigma")
sigma.cauchy.1 <- extract.samples(m8.2.cauchy.1, pars="sigma")
sigma.cauchy.point.1 <- extract.samples(m8.2.cauchy.point.1, pars="sigma")
dens(sigma.cauchy.10[[1]], xlab="sigma", col="red")
dens(sigma.cauchy.1[[1]], add=TRUE, col="blue")
dens(sigma.cauchy.point.1[[1]], add=TRUE, col="green")

# plot the posterior distribution for sigma under the exponential priors
sigma.exp.10 <- extract.samples(m8.2.exp.10, pars="sigma")
sigma.exp.1 <- extract.samples(m8.2.exp.1, pars="sigma")
sigma.exp.point.1 <- extract.samples(m8.2.exp.point.1, pars="sigma")
dens(sigma.exp.10[[1]], xlab="sigma", col="red")
dens(sigma.exp.1[[1]], add=TRUE, col="blue")
dens(sigma.exp.point.1[[1]], add=TRUE, col="green")

## 8M3

# estimate the terrain ruggedness model with varying values for warmup
m <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2)
  ), data=dd.trim )

m.warmup.1 <- map2stan(m, chains = 4, cores = 4, warmup = 1, iter = 1000)
m.warmup.5 <- map2stan(m, chains = 4, cores = 4, warmup = 5, iter = 1000)
m.warmup.10 <- map2stan(m, chains = 4, cores = 4, warmup = 10, iter = 1000)
m.warmup.50 <- map2stan(m, chains = 4, cores = 4, warmup = 50, iter = 1000)
m.warmup.100 <- map2stan(m, chains = 4, cores = 4, warmup = 100, iter = 1000)
m.warmup.500 <- map2stan(m, chains = 4, cores = 4, warmup = 500, iter = 1000)
m.warmup.1000 <- map2stan(m, chains = 4, cores = 4, warmup = 1000, iter = 1000)

precis(m.warmup.1)
precis(m.warmup.5)
precis(m.warmup.10)
precis(m.warmup.50)
precis(m.warmup.100)
precis(m.warmup.500)
precis(m.warmup.1000)

## 8H1

mp <- map2stan(
  alist(
    a ~ dnorm(0, 1),
    b ~ dcauchy(0, 1)
  ),
  data = list(y = 1),
  start = list(a = 0, b = 0),
  iter = 1e4,
  warmup = 100,
  WAIC = FALSE
)

# extract samples for a and b
trials <- 1e4
a.samples <- extract.samples(mp, pars="a", n = trials)
b.samples <- extract.samples(mp, pars="b", n = trials)

plot(x = seq(from = 1, to = trials, length.out = trials), y = a.samples$a, ylim = c(-4, 4))
lines(seq(from = 1, to = trials, length.out = trials), a.samples$a)

plot(x = seq(from = 1, to = trials, length.out = trials), y = b.samples$b, ylim = c(-50, 50))
lines(seq(from = 1, to = trials, length.out = trials), b.samples$b)

## 8H2

# refit the divorce rate models from chapter 5 using map2stan
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
d$MedianAgeMarriage_s <- (d$MedianAgeMarriage-mean(d$MedianAgeMarriage))/
  sd(d$MedianAgeMarriage)
d$Marriage_s <- (d$Marriage - mean(d$Marriage))/sd(d$Marriage)
df <- d[, c("Divorce", "MedianAgeMarriage_s", "Marriage_s")]

m5.1_stan <- map2stan(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bA * MedianAgeMarriage_s ,
    a ~ dnorm( 10 , 10 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ),
  data = df , chains=4 )
m5.2_stan <- map2stan(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bR * Marriage_s ,
    a ~ dnorm( 10 , 10 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ),
  data = df , chains=4 )
m5.3_stan <- map2stan(
  alist(
    Divorce ~ dnorm( mu , sigma ) ,
    mu <- a + bR*Marriage_s + bA*MedianAgeMarriage_s ,
    a ~ dnorm( 10 , 10 ) ,
    bR ~ dnorm( 0 , 1 ) ,
    bA ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ),
  data = df , chains=4 )

# compare the resulting models
compare(m5.1_stan,m5.2_stan,m5.3_stan)

## 8H4
N <- 100
height <- rnorm(N, 10, 2)
leg_prop <- runif(N, .4, .5)
leg_left <- leg_prop*height + rnorm(N, 0, .02)
leg_right <- leg_prop*height + rnorm(N, 0, .02)
d <- data.frame(height, leg_left, leg_right)

# fit the same model as in chapter 5
m5.8s <- map2stan(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left + br*leg_right,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2, 10),
    sigma ~ dcauchy(0, 1)
  ),
  data = d,
  chains = 4,
  start = list(a = 10, bl = 0, br = 0, sigma = 1)
)

# fit the same model but make the prior for `br` strictly positive
m5.8s2 <- map2stan(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + bl*leg_left + br*leg_right,
    a ~ dnorm(10, 100),
    bl ~ dnorm(2, 10),
    br ~ dnorm(2, 10) & T[0,],
    sigma ~ dcauchy(0, 1)
  ),
  data = d,
  chains = 4,
  start = list(a = 10, bl = 0, br = 0, sigma = 1)
)

# compare the results (we wish we could `plot` it as well, but alas the S4-vector-method error persists)
compare(m5.8s, m5.8s2)
precis(m5.8s)
precis(m5.8s2)
