# notes

library(rethinking)

## 6.15
data(cars)
m <- map(
  alist(
    dist ~ dnorm(mu,sigma),
    mu <- a + b*speed,
    a ~ dnorm(0,100),
    b ~ dnorm(0,10),
    sigma ~ dunif(0,30)
  ) ,
  data=cars )
post <- extract.samples(m,n=1000)

## 6.16 - compute the log likelihood of each observation at each sample from the posterior
n.samples <- 1:1000
log.likelihood <- sapply(X = n.samples, FUN = function(sample) {
  mu <- post$a[sample] + post$b[sample]*cars$speed
  return( dnorm(x = cars$dist, mean = mu, sd = post$sigma[sample], log = TRUE) )
})

## 6.17 - compute the "llpd" for the "WAIC"
n.cases <- nrow(cars)
llpd <- sapply(X = n.cases, FUN = function(i) { log_sum_exp(x = log.likelihood[i,]) - log(n.samples) })

## 6.18 - compute the "p_WAIC" for the "WAIC"
p.waic <- sapply(X = n.cases, FUN = function(i) { var(log.likelihood[i,]) })

## 6.19 piece it together and compute the WAIC
waic <- -2*(sum(llpd) - sum(p.waic))

## 6.21 - load milk data
data(milk)
d <- milk[ complete.cases(milk) , ]
d$neocortex <- d$neocortex.perc / 100
dim(d)

## 6.22
a.start <- mean(d$kcal.per.g)
sigma.start <- log(sd(d$kcal.per.g))
m6.11 <- map(
  alist(
    kcal.per.g ~ dnorm( a , exp(log.sigma) )
  ),
  data=d , start=list(a=a.start,log.sigma=sigma.start) )
m6.12 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
    mu <- a + bn*neocortex
  ),
  data=d , start=list(a=a.start,bn=0,log.sigma=sigma.start) )
m6.13 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
    mu <- a + bm*log(mass)
  ),
  data=d , start=list(a=a.start,bm=0,log.sigma=sigma.start) )
m6.14 <- map(
  alist(
    kcal.per.g ~ dnorm( mu , exp(log.sigma) ) ,
    mu <- a + bn*neocortex + bm*log(mass)
  ),
  data=d , start=list(a=a.start,bn=0,bm=0,log.sigma=sigma.start) )

## 6.29 - compute counterfactual predictions

# neocortex from 0.5 to 0.8
nc.seq <- seq(from=0.5,to=0.8,length.out=30)
d.predict <- list(
  kcal.per.g = rep(0,30), # empty outcome
  neocortex = nc.seq,     # sequence of neocortex
  mass = rep(4.5,30)      # average mass
)
pred.m6.14 <- link( m6.14 , data=d.predict )
mu <- apply( pred.m6.14 , 2 , mean )
mu.PI <- apply( pred.m6.14 , 2 , PI )

# plot it all
plot( kcal.per.g ~ neocortex , d , col=rangi2 )
lines( nc.seq , mu , lty=2 )
lines( nc.seq , mu.PI[1,] , lty=2 )
lines( nc.seq , mu.PI[2,] , lty=2 )

## 6.30 - ensemble the models
milk.ensemble <- ensemble( m6.11 , m6.12 , m6.13 , m6.14 , data=d.predict )
mu <- apply( milk.ensemble$link , 2 , mean )
mu.PI <- apply( milk.ensemble$link , 2 , PI )
lines( nc.seq , mu )
shade( mu.PI , nc.seq )
