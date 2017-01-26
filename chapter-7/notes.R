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
