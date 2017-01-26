# notes

library(rethinking)

## 7.1
data(rugged)
d <- rugged

# make log version of outcome
d$log_gdp <- log( d$rgdppc_2000 )

# extract countries with GDP data
dd <- d[complete.cases(d$rgdppc_2000),]

# split countries into Africa and not-Africa
d.A1 <- dd[ dd$cont_africa==1 , ]
d.A0 <- dd[ dd$cont_africa==0 , ]

## 7.2

# african countries
m7.1 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + beta.rugged*rugged ,
    a ~ dnorm( 8 , 100 ) ,
    beta.rugged ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 ) ),
  data=d.A1 )

# non-african countries
m7.2 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + beta.rugged*rugged ,
    a ~ dnorm( 8 , 100 ) ,
    beta.rugged ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ), 
  data=d.A0 )

# plot posterior predictions and 97% prediction intervals around the mean
n.trials <- 1e4
beta.rugged.seq <- seq(from = 0, to = 10, length.out = 50)
prediction.data <- data.frame(rugged = beta.rugged.seq)

computeMu <- function(model, data, n.trials) {
  mu <- link(fit = model, data = data, n = n.trials)
  return(mu)
}

computeMuMean <- function(mu) {
  mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
  return(mu.mean)
}

computeMuPI <- function(mu) {
  mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI, prob = .97)
  return(mu.PI)
}

simulateGDP <- function(model, prediction.data) {
  simulated.GDP <- sim(fit = model, data = prediction.data)
  return(simulated.GDP)
}

plotResults <- function(model, prediction.data, original.data, n.trials) {
  mu <- computeMu(model, prediction.data, n.trials)
  mu.mean <- computeMuMean(mu)
  mu.PI <- computeMuPI(mu)
  simulated.GDP <- simulateGDP(model = model, prediction.data = prediction.data)
  simulated.GDP.PI <- apply(X = simulated.GDP, MARGIN = 2, FUN = PI)
  plot(log_gdp ~ rugged, data = original.data, col = rangi2)
  lines(x = prediction.data$rugged, y = mu.mean, lty = 2)
  shade(object = mu.PI, lim = prediction.data$rugged)
}

## plot results
plotResults(model = m7.1, prediction.data = prediction.data, original.data = d.A1, n.trials = n.trials)
plotResults(model = m7.2, prediction.data = prediction.data, original.data = d.A0, n.trials = n.trials)

## 7.3
m7.3 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + beta.rugged*rugged ,
    a ~ dnorm( 8 , 100 ) ,
    beta.rugged ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 ) ),
  data=dd )

## 7.4
m7.4 <- map(
  alist(
    log_gdp ~ dnorm( mu , sigma ) ,
    mu <- a + beta.rugged*rugged + beta.africa*cont_africa ,
    a ~ dnorm( 8 , 100 ) ,
    beta.rugged ~ dnorm( 0 , 1 ) ,
    beta.africa ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ), data=dd )

## 7.5
compare( m7.3, m7.4 )

## 7.6
rugged.seq <- seq(from=-1,to=8,by=0.25)

# compute mu over samples, fixing cont_africa=0
mu.NotAfrica <- link( m7.4 , data=data.frame(cont_africa=0,rugged=rugged.seq) )

# compute mu over samples, fixing cont_africa=1
mu.Africa <- link( m7.4 , data=data.frame(cont_africa=1,rugged=rugged.seq) )

# summarize to means and intervals
mu.NotAfrica.mean <- apply( mu.NotAfrica , 2 , mean )
mu.NotAfrica.PI <- apply( mu.NotAfrica , 2 , PI , prob=0.97 )
mu.Africa.mean <- apply( mu.Africa , 2 , mean )
mu.Africa.PI <- apply( mu.Africa , 2 , PI , prob=0.97 )

## 7.7 - fit a model that includes an interaction between ruggedness and that nation being in Africa
m7.5 <- map(
  alist(
    log_gdp ~ dnorm( mu, sigma ),
    mu <- a + gamma.rugged*rugged + beta.africa*cont_africa,
    gamma.rugged <- alpha.gamma.rugged + beta.gamma.rugged*cont_africa,
    a ~ dnorm( 8, 100 ),
    beta.africa ~ dnorm( 0, 1 ),
    alpha.gamma.rugged ~ dnorm( 0, 1 ),
    beta.gamma.rugged ~ dnorm( 0, 1 ),
    sigma ~ dunif( 0, 10 )
  ), data = dd
)

## 7.8 - compare models
compare( m7.3, m7.4, m7.5 )

## 7.10
rugged.seq <- seq(from=-1,to=8,by=0.25)
mu.Africa <- link( m7.5 , data=data.frame(cont_africa=1,rugged=rugged.seq) )
mu.Africa.mean <- apply( mu.Africa , 2 , mean )
mu.Africa.PI <- apply( mu.Africa , 2 , PI , prob=0.97 )
mu.NotAfrica <- link( m7.5 , data=data.frame(cont_africa=0,rugged=rugged.seq) )
mu.NotAfrica.mean <- apply( mu.NotAfrica , 2 , mean )
mu.NotAfrica.PI <- apply( mu.NotAfrica , 2 , PI , prob=0.97 )

## 7.11

# plot African nations with regression
d.A1 <- dd[dd$cont_africa==1,]
plot( log(rgdppc_2000) ~ rugged , data=d.A1 ,
      col=rangi2 , ylab="log GDP year 2000" ,
      xlab="Terrain Ruggedness Index" )
mtext( "African nations" , 3 )
lines( rugged.seq , mu.Africa.mean , col=rangi2 )
shade( mu.Africa.PI , rugged.seq , col=col.alpha(rangi2,0.3) )

# plot non-African nations with regression
d.A0 <- dd[dd$cont_africa==0,]
plot( log(rgdppc_2000) ~ rugged , data=d.A0 ,
      col="black" , ylab="log GDP year 2000" ,
      xlab="Terrain Ruggedness Index" )
mtext( "Non-African nations" , 3 )
lines( rugged.seq , mu.NotAfrica.mean )
shade( mu.NotAfrica.PI , rugged.seq )

## 7.13
posterior.samples <- extract.samples(m7.5)
gamma.africa <- posterior.samples$alpha.gamma.rugged + posterior.samples$beta.gamma.rugged*1
gamma.not.africa <- posterior.samples$alpha.gamma.rugged + posterior.samples$beta.gamma.rugged*0

## 7.14
mean(gamma.africa)
mean(gamma.not.africa)

## 7.15
dens( gamma.africa, xlim=c(-0.5,0.6), ylim=c(0,5.5), xlab="gamma", col=rangi2 )
dens( gamma.not.africa , add=TRUE )

## 7.16
diff <- gamma.africa - gamma.not.africa
mean(diff < 0)

## 7.18
data(tulips)
d <- tulips
str(d)