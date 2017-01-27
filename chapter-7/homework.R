# homework

## 7M1

# The fact that tulips cease to grow entirely when the temperature is hot, in lieu of shrinking in bloom-size in a linear fashion, implies a non-linear model.
# This said, we can point to an interaction between water, shade (which together were shown to be conducive to tulip growth) and temperature.

## 7M2

# A regression equation that would make the bloom size zero whenever the temperature is hot would include only terms interacted with an is_not_hot variable, such that when
# this variable is 0, i.e. it is *not* is_not_hot, the bloom size would be zero.

## 7M3

# The relationship between the amount of food that a raven consumes and the presence of a wolf killing this prey cannot be entirely linear, as without the wolf the
# raven would not eat any food at all. Once the wolf does enter the picture, though, the relationship could thereafter be a linear one.

## 7H1
library(rethinking)
data(tulips)
d <- tulips
str(d)

# center variables
d$water.centered <- d$water - mean(d$water)
d$shade.centered <- d$shade - mean(d$shade)
d$bed.b <- ifelse(d$bed == "b", 1, 0)
d$bed.c <- ifelse(d$bed == "c", 1, 0)

# fit model with interaction term plus main effects for unique levels of `bed`
m7H1 <- map(
  alist(
    blooms ~ dnorm( mu , sigma ) ,
    mu <- alpha + beta.water*water.centered + beta.shade*shade.centered + beta.water.shade*water*shade + beta.bed.b*bed.b + beta.bed.c*bed.c,
    c(alpha, beta.water, beta.shade, beta.water.shade, beta.bed.b, beta.bed.c) ~ dnorm( 0, 100 ),
    sigma ~ dunif( 0 , 100 )
  ),
  data=d,
  start=list(alpha=mean(d$blooms), beta.water=0, beta.shade=0, beta.water.shade=0, beta.bed.b=0, beta.bed.c=0, sigma=sd(d$blooms)), 
  control=list(maxit=1e4)
)

## 7H2
m7H2 <- map(
  alist(
    blooms ~ dnorm( mu , sigma ) ,
    mu <- alpha + beta.water*water.centered + beta.shade*shade.centered + beta.water.shade*water*shade,
    alpha ~ dnorm(130, 100),
    c(beta.water, beta.shade, beta.water.shade) ~ dnorm( 0, 100 ),
    sigma ~ dunif( 0 , 100 )
  ),
  data=d,
  start=list(alpha=mean(d$blooms), beta.water=0, beta.shade=0, beta.water.shade=0, sigma=sd(d$blooms)), 
  control=list(maxit=1e4)
)

compare(m7H1, m7H2)
posterior.samples <- extract.samples(m7H1)
hist(posterior.samples$beta.bed.b)
hist(posterior.samples$beta.bed.c)
hist(posterior.samples$alpha)

## 7H3
data(rugged)
d <- rugged
d <- d[complete.cases(d$rgdppc_2000),]
d$log_gdp <- log(d$rgdppc_2000)
d$rugged.centered <- d$rugged - mean(d$rugged)

# a.

## fit model with all data
m.7H3.all <- map(
  alist(
    log_gdp ~ dnorm( mu, sigma ),
    mu <- alpha + beta.rugged*rugged + beta.cont_africa*cont_africa + beta.rugged.cont_africa*rugged*cont_africa,
    alpha ~ dnorm(8.5, 5),
    c(beta.rugged, beta.cont_africa, beta.rugged.cont_africa) ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data=d
)

## fit model with all data except that of the Seychelles
m.7H3.except.seychelles <- map(
  alist(
    log_gdp ~ dnorm( mu, sigma ),
    mu <- alpha + beta.rugged*rugged + beta.cont_africa*cont_africa + beta.rugged.cont_africa*rugged*cont_africa,
    alpha ~ dnorm(8.5, 5),
    c(beta.rugged, beta.cont_africa, beta.rugged.cont_africa) ~ dnorm(0, 10),
    sigma ~ dunif(0, 10)
  ), data=d[(d$country != "Seychelles"),]
)

## create triptych plot to examine whether the effect of ruggedness on log-GDP still does depend on continent
plotTriptych <- function(model) {
  par(mfrow=c(1,2))
  rugged.seq <- 0:8

  # loop over values of water.c and plot predictions
  for ( is.africa in 0:1 ) {
    dt <- d[d$cont_africa==is.africa,]
    plot( log_gdp ~ rugged , data=dt , col=rangi2 ,
          main=paste("cont_africa =", is.africa) , xlim=c(0, 8) , ylim=c(5.5,10) ,
          xlab="rugged" )
    mu <- link( model , data=data.frame(cont_africa=is.africa, rugged=rugged.seq) )
    mu.mean <- apply( mu , 2 , mean )
    mu.PI <- apply( mu , 2 , PI , prob=0.97 )
    lines( rugged.seq , mu.mean )
    lines( rugged.seq , mu.PI[1,] , lty=2 )
    lines( rugged.seq , mu.PI[2,] , lty=2 )
  }
}

plotTriptych(model = m.7H3.all)
plotTriptych(model = m.7H3.except.seychelles)

## compute MAP values for the interaction coefficient at cont_africa=1 for each of the models
coef.m.7H3.all <- coef(m.7H3.all)
coef.m.7H3.except.seychelles <- coef(m.7H3.except.seychelles)

coef.m.7H3.all["beta.rugged"] + coef.m.7H3.all["beta.rugged.cont_africa"]*1
coef.m.7H3.except.seychelles["beta.rugged"] + coef.m.7H3.except.seychelles["beta.rugged.cont_africa"]*1

## analysis

# Both with and without the Seychelles, ruggedness and log-GDP continue to exhibit a negative relationship when cont_africa=0. However, in the latter case, the relationship
# between ruggedness and log-GDP when cont_africa=1 does remain positive, but decreases in slope, indicating that the Seychelles appears to have played a significant role in
# this upward-sloping relationship from the outset.

# b.

# Covered in a.

# c.
d.except.seychelles <- d[(d$country != "Seychelles"),]

m.7H3.c.1 <- map(
  alist(
    log_gdp ~ dnorm( mu, sigma ),
    mu <- alpha + beta.rugged*rugged.centered,
    alpha ~ dnorm(8.5, 5),
    beta.rugged ~ dnorm(0, 100),
    sigma ~ dunif(0, 10)
  ), data=d.except.seychelles
)

m.7H3.c.2 <- map(
  alist(
    log_gdp ~ dnorm( mu, sigma ),
    mu <- alpha + beta.rugged*rugged.centered + beta.cont_africa*cont_africa,
    alpha ~ dnorm(8.5, 5),
    c(beta.rugged, beta.cont_africa) ~ dnorm(0, 100),
    sigma ~ dunif(0, 10)
  ), data=d.except.seychelles
)

m.7H3.c.3 <- map(
  alist(
    log_gdp ~ dnorm( mu, sigma ),
    mu <- alpha + beta.rugged*rugged.centered + beta.cont_africa*cont_africa + beta.rugged.cont_africa*rugged.centered*cont_africa,
    alpha ~ dnorm(8.5, 5),
    c(beta.rugged, beta.cont_africa, beta.rugged.cont_africa) ~ dnorm(0, 100),
    sigma ~ dunif(0, 10)
  ), data=d.except.seychelles
)

compare(m.7H3.c.1, m.7H3.c.2, m.7H3.c.3)

## generate WAIC-averaged predictions
rugged.seq <- seq(from = 0, to = 8, by = .1)
rugged.seq.centered <- rugged.seq - mean(rugged.seq)
prediction.data = data.frame(rugged.centered = rugged.seq.centered, cont_africa = 1)
mu.ensemble <- ensemble(m.7H3.c.1, m.7H3.c.2, m.7H3.c.3, data = prediction.data)
mu.mean <- apply(X = mu.ensemble$link, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu.ensemble$link, MARGIN = 2, FUN = PI)
data.plot <- d[(d$cont_africa == 1),]
plot(log_gdp ~ rugged.centered, data=data.plot, pch=ifelse(data.plot$country == "Seychelles", 16, 1), col="slateblue")
lines( rugged.seq.centered, mu.mean )
lines( rugged.seq.centered, mu.PI[1,], lty=2 )
lines( rugged.seq.centered, mu.PI[2,], lty=2 )
