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
