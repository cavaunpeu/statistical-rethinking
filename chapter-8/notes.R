# notes

## 8.1
num_weeks <- 1e5
positions <- rep(0,num_weeks)
current <- 10
for ( i in 1:num_weeks ) {
  # record current position
  positions[i] <- current
  # flip coin to generate proposal
  proposal <- current + sample( c(-1,1) , size=1 )
  # now make sure he loops around the archipelago
  if ( proposal < 1 ) proposal <- 10
  if ( proposal > 10 ) proposal <- 1
  # move?
  prob_move <- proposal/current
  current <- ifelse( runif(1) < prob_move , proposal , current )
}

## 8.2
library(rethinking)
data(rugged)
d <- rugged
d$log_gdp <- log(d$rgdppc_2000)
dd <- d[ complete.cases(d$rgdppc_2000) , ]

## 8.3
m8.1 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dunif(0,10)
  ),
  data=dd )

precis(m8.1)

## 8.4
dd.trim <- dd[ , c("log_gdp", "rugged", "cont_africa")]

## 8.5
m8.1stan <- map2stan(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + bR*rugged + bA*cont_africa + bAR*rugged*cont_africa ,
    a ~ dnorm(0,100),
    bR ~ dnorm(0,10),
    bA ~ dnorm(0,10),
    bAR ~ dnorm(0,10),
    sigma ~ dcauchy(0,2)
  ), data=dd.trim )

## 8.7
m8.1stan_4chains <- map2stan(m8.1stan, chains = 4, cores = 4)
precis(m8.1stan_4chains)

## 8.8
posterior.samples <- extract.samples(m8.1stan)

## 8.10
pairs(m8.1)

## 8.11
show(m8.1stan)

## 8.17
y <- rnorm( 100 , mean=0 , sd=1 )

## 8.18
m8.4 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a1 + a2 ,
    sigma ~ dcauchy( 0 , 1 )
  ),
  data=list(y=y) , start=list(a1=0,a2=0,sigma=1) , chains=2 , iter=4000 , warmup=1000 )
precis(m8.4)

## 8.19
m8.5 <- map2stan(
  alist(
    y ~ dnorm( mu , sigma ) ,
    mu <- a1 + a2 ,
    a1 ~ dnorm( 0 , 10 ) ,
    a2 ~ dnorm( 0 , 10 ) ,
    sigma ~ dcauchy( 0 , 1 )
  ),
  data=list(y=y) , start=list(a1=0,a2=0,sigma=1) , chains=2 , iter=4000 , warmup=1000 )
precis(m8.5)
