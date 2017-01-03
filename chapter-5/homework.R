# homework

## 5M1 - simulate spurious correlations

# generate observations
trials <- 1e3
x.real.correlation <- rnorm(n = trials, mean = 1, sd = 1)
x.spurious.correlation <- rnorm(n = trials, mean = x.real.correlation)
y <- rnorm(n = trials, mean = x.real.correlation)

# plot correlations
df <- data.frame(y = y, x.real.correlation = x.real.correlation, x.spurious.correlation = x.spurious.correlation)
pairs(df)

# buid model, inspect results
model <- lm(y ~ x.real.correlation + x.spurious.correlation)
precis(model)

## 5M2 - simulate masked variable relationships

# generate observations
trials <- 1e3
rho <- .7
x.postive <- rnorm(n = trials)
x.negative <- rnorm(n = trials, mean = x.postive * rho, sd = sqrt(1 - rho^2))
y <- rnorm(n = trials, mean = x.postive - x.negative)

# plot correlations
df <- data.frame(y = y, x.postive = x.postive, x.negative = x.negative)
pairs(df)

# build models, inspect results
model <- lm(y ~ x.postive + x.negative)
precis(model)

## 5M4 - predict divroce as a function of marriage rate, median age at marriage, and percent LDS population

# load data
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
d$pct_LDS <- c(0.75, 4.53, 6.18, 1, 2.01, 2.82, 0.43, 0.55, 0.38,
               0.75, 0.82, 5.18, 26.35, 0.44, 0.66, 0.87, 1.25, 0.77, 0.64, 0.81,
               0.72, 0.39, 0.44, 0.58, 0.72, 1.14, 4.78, 1.29, 0.61, 0.37, 3.34,
               0.41, 0.82, 1.48, 0.52, 1.2, 3.85, 0.4, 0.37, 0.83, 1.27, 0.75,
               1.21, 67.97, 0.74, 1.13, 3.99, 0.92, 0.44, 11.5 )

# standardize variables
d$Marriage.standardized <- (d$Marriage - mean(d$Marriage)) / sd(d$Marriage)
d$MedianAgeMarriage.standardized <- (d$MedianAgeMarriage - mean(d$MedianAgeMarriage)) / sd(d$MedianAgeMarriage)
d$pct_LDS.standardized <- (d$pct_LDS - mean(d$pct_LDS)) / sd(d$pct_LDS)

# build a model, inspect results
model <- map(
  alist(
    Divorce ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta.marriage.rate * Marriage.standardized + beta.median.age.at.marriage * MedianAgeMarriage.standardized + beta.lds * pct_LDS.standardized,
    alpha ~ dnorm(mean = 0, sd = 100),
    c(beta.marriage.rate, beta.median.age.at.marriage, beta.lds) ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, 10)
  ),
  data = d
)
precis(model)

## 5H1
library(rethinking)
data(foxes)
d <- foxes

# model body weight as a linear function of territory size
m1 <- map(
  alist(
    weight ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta*area,
    alpha ~ dnorm(mean = 0, sd = 100),
    beta ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, max = 10)
  ),
  data = d
)

# compute posterior statistics
area.seq <- seq(from = 0, to = 6, length.out = 100)
mu <- link(m1, data = data.frame(area = area.seq))
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI, prob = .89)

# plot results
plot(weight ~ area, data = d)
lines(x = area.seq, y = mu.mean)
shade(object = mu.PI, lim = area.seq)

# model body weight as a linear function of group size
m2 <- map(
  alist(
    weight ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta*groupsize,
    alpha ~ dnorm(mean = 0, sd = 100),
    beta ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, max = 10)
  ),
  data = d
)

# compute posterior statistics
groupsize.seq <- seq(from = 1, to = 9, length.out = 100)
mu <- link(m2, data = data.frame(groupsize = groupsize.seq))
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI, prob = .89)

# plot results
plot(weight ~ groupsize, data = d)
lines(x = groupsize.seq, y = mu.mean)
shade(object = mu.PI, lim = groupsize.seq)

## 5H2 - model body weight as a linear function of both territory size and group size
m3 <- map(
  alist(
    weight ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta.group.size*groupsize + beta.area*area,
    alpha ~ dnorm(mean = 0, sd = 100),
    c(beta.group.size, beta.area) ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, max = 10)
  ),
  data = d
)
precis(m3)

# plot territory size vs. weight holding groupsize constant at its mean
mean.groupsize <- mean(d$groupsize)
pred.data <- data.frame(
  area = area.seq,
  groupsize = mean.groupsize
)

mu <- link(m3, data = pred.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI, prob = .89)

plot(weight ~ area, data = d, type = "n")
lines(x = area.seq, y = mu.mean)
lines(x = area.seq, y = mu.PI[1,], lty = 2)
lines(x = area.seq, y = mu.PI[2,], lty = 2)
title('weight vs. territory size while holding\n groupsize constant at its mean')

# plot groupsize vs. weight holding territory size constant at its mean
mean.area <- mean(d$area)
pred.data <- data.frame(
  area = mean.area,
  groupsize = groupsize.seq
)

mu <- link(m3, data = pred.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI, prob = .89)

plot(weight ~ groupsize, data = d, type = "n")
lines(x = groupsize.seq, y = mu.mean)
lines(x = groupsize.seq, y = mu.PI[1,], lty = 2)
lines(x = groupsize.seq, y = mu.PI[2,], lty = 2)
title('weight vs. groupsize while holding\n area constant at its mean')

## 5H3

# model weight as a linear function of average food and group size
m4 <- map(
  alist(
    weight ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta.group.size*groupsize + beta.avgfood*avgfood,
    alpha ~ dnorm(mean = 0, sd = 100),
    c(beta.group.size, beta.avgfood) ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, max = 10)
  ),
  data = d
)

# model weight as a linear function of average food, group size and area
m5 <- map(
  alist(
    weight ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta.group.size*groupsize + beta.avgfood*avgfood + beta.area*area,
    alpha ~ dnorm(mean = 0, sd = 100),
    c(beta.group.size, beta.avgfood, beta.area) ~ dnorm(mean = 0, sd = 10),
    sigma ~ dunif(min = 0, max = 10)
  ),
  data = d
)

# plot weight as a function of average food, holding group size and area at their respective means
mean.area <- mean(d$area)
mean.groupsize <- mean(d$groupsize)
avgfood.seq <- seq(from = .25, to = 1.5, length.out = 500)
pred.data <- data.frame(
  area = mean.area,
  groupsize = mean.groupsize,
  avgfood = avgfood.seq
)

mu <- link(m5, data = pred.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI, prob = .89)

plot(weight ~ avgfood, data = d, type = "n")
lines(x = avgfood.seq, y = mu.mean)
lines(x = avgfood.seq, y = mu.PI[1,], lty = 2)
lines(x = avgfood.seq, y = mu.PI[2,], lty = 2)
title('weight vs. avgfood while holding\n area and group size constant at their\n respective means')

# plot weight as a function of area, holding group size and average food at their respective means
mean.avgfood <- mean(d$avgfood)
mean.groupsize <- mean(d$groupsize)
area.seq <- seq(from = 0, to = 6, length.out = 500)
pred.data <- data.frame(
  area = area.seq,
  groupsize = mean.groupsize,
  avgfood = mean.avgfood
)

mu <- link(m5, data = pred.data)
mu.mean <- apply(X = mu, MARGIN = 2, FUN = mean)
mu.PI <- apply(X = mu, MARGIN = 2, FUN = PI, prob = .89)

plot(weight ~ area, data = d, type = "n")
lines(x = area.seq, y = mu.mean)
lines(x = area.seq, y = mu.PI[1,], lty = 2)
lines(x = area.seq, y = mu.PI[2,], lty = 2)
title('weight vs. area holding\n avgfood and group size constant at their\n respective means')