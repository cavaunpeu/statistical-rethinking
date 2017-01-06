# homework

## 6M1

# AIC
# Reliable when:
#   1. The priors are flat or overwhelmed by the likelihood.
#   2. The posterior is approximately multivariate Gaussian.
#   3. The sample size N is much larger than the number of parameters k.
#
# # DIC
# Like AIC, but aware of informative priors.
# The flexibility of the model is measured as the difference between the average deviance and the deviance at the posterior mean.

# # WAIC
# Like DIC, but better, and handles uncertainty on a case-by-case basis.

# Comparisons:
#   1. From most general to least general: WAIC, DIC, AIC.
#   2. When posterior predictive mean is a good representation of the posterior predictive distribution, WAIC and DIC will tend to agree.
#   3. When priors are effectively flat or overwhelmed by the amount of data, the DIC and AIC will tend to agree.

## 6M2

# Model selection refers to selecting one (or several) models of many. Model averaging refers to weighting the predictions of many models by some relative information criterion in order to create an aggregate posterior prediction distribution.
#
# Under model selection, the strength of models that were almost as good is lost. Said differently, it discards model uncertainty. Under model averaging, we're often able to create the same ensembled posterior predictive distribution from two different sets of models.
# As such, information criteria values of each model (in each model set) is by definition somewhere lost.

## 6M3

# When comparing models with different information criteria, if all models are not fit to the exact same observations than the comparisons are erroneous. Models with fewer observations have an "easier job," and therefore have lower (better) information criterion,
# as deviance (and therefore information criteria) is an additive measure of error.

## 6M4

# As a prior becomes more concentrated, the effective number of parameters goes down as measured by DIC or WAIC.

## 6M5

# Informative priors instruct our model to "not get too excited by the data." In other words, they tell our models not to overfit!

## 6M6

# If a prior is overly informative, even the regular features of a sample will not be learned by our model.

## 6H1

# load data
library(rethinking)
data("Howell1")
d <- Howell1
d$age <- (d$age - mean(d$age)) / sd(d$age)

# split dataframe into 2 parts
set.seed(1000)
split.index <- sample(x = 1:nrow(d), size = nrow(d)/2)
d1 <- d[split.index,]
d2 <- d[-split.index,]

# fit a bunch of models

## define function lists
f1 <- alist(
    height ~ dnorm(mean = mu, sd = sigma),
    mu <- alpha + beta.1*age,
    c(alpha, beta.1) ~ dnorm(mean = 0, sd = 100),
    sigma ~ dunif(min = 0, max = 50)
)

f2 <- alist(
  height ~ dnorm(mean = mu, sd = sigma),
  mu <- alpha + beta.1*age + beta.2*age^2,
  c(alpha, beta.1, beta.2) ~ dnorm(mean = 0, sd = 100),
  sigma ~ dunif(min = 0, max = 50)
)

f3 <- alist(
  height ~ dnorm(mean = mu, sd = sigma),
  mu <- alpha + beta.1*age + beta.2*age^2 + beta.3*age^3,
  c(alpha, beta.1, beta.2, beta.3) ~ dnorm(mean = 0, sd = 100),
  sigma ~ dunif(min = 0, max = 50)
)

f4 <- alist(
  height ~ dnorm(mean = mu, sd = sigma),
  mu <- alpha + beta.1*age + beta.2*age^2 + beta.3*age^3 + beta.4*age^4,
  c(alpha, beta.1, beta.2, beta.3, beta.4) ~ dnorm(mean = 0, sd = 100),
  sigma ~ dunif(min = 0, max = 50)
)

f5 <- alist(
  height ~ dnorm(mean = mu, sd = sigma),
  mu <- alpha + beta.1*age + beta.2*age^2 + beta.3*age^3 + beta.4*age^4 + beta.5*age^5,
  c(alpha, beta.1, beta.2, beta.3, beta.4, beta.5) ~ dnorm(mean = 0, sd = 100),
  sigma ~ dunif(min = 0, max = 50)
)

f6 <- alist(
  height ~ dnorm(mean = mu, sd = sigma),
  mu <- alpha + beta.1*age + beta.2*age^2 + beta.3*age^3 + beta.4*age^4 + beta.5*age^5 + beta.6*age^6,
  c(alpha, beta.1, beta.2, beta.3, beta.4, beta.5, beta.6) ~ dnorm(mean = 0, sd = 100),
  sigma ~ dunif(min = 0, max = 50)
)

## pick some sensible starting values for alpha and sigma
alpha.start <- mean(d$height)
sigma.start <- sd(d$height)

## fit our models
m1 <- map(flist = f1, data = d1, start = list(alpha = alpha.start, sigma = sigma.start, beta.1 = 0))
m2 <- map(flist = f2, data = d1, start = list(alpha = alpha.start, sigma = sigma.start, beta.1 = 0, beta.2 = 0))
m3 <- map(flist = f3, data = d1, start = list(alpha = alpha.start, sigma = sigma.start, beta.1 = 0, beta.2 = 0, beta.3 = 0))
m4 <- map(flist = f4, data = d1, start = list(alpha = alpha.start, sigma = sigma.start, beta.1 = 0, beta.2 = 0, beta.3 = 0, beta.4 = 0))
m5 <- map(flist = f5, data = d1, start = list(alpha = alpha.start, sigma = sigma.start, beta.1 = 0, beta.2 = 0, beta.3 = 0, beta.4 = 0, beta.5 = 0))
m6 <- map(flist = f6, data = d1, start = list(alpha = alpha.start, sigma = sigma.start, beta.1 = 0, beta.2 = 0, beta.3 = 0, beta.4 = 0, beta.5 = 0, beta.6 = 0))

# compare the models
compare(m1, m2, m3, m4, m5, m6)

## 6H2

# for each model, compute the model-averaged mean and 97% prediction interval, and plot against the raw data

## build some helper functions
n.trials <- 1e4
age.seq <- seq(from = -2, to = 3.5, length.out = 58)
prediction.data <- data.frame(age = age.seq)

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

simulateHeights <- function(model, prediction.data) {
  simulated.heights <- sim(fit = model, data = prediction.data)
  return(simulated.heights)
}

plotResults <- function(model, prediction.data, original.data, n.trials) {
  mu <- computeMu(model, prediction.data, n.trials)
  mu.mean <- computeMuMean(mu)
  mu.PI <- computeMuPI(mu)
  simulated.heights <- simulateHeights(model = model, prediction.data = prediction.data)
  simulated.heights.PI <- apply(X = simulated.heights, MARGIN = 2, FUN = PI)
  plot(height ~ age, data = original.data, col = rangi2)
  lines(x = prediction.data$age, y = mu.mean, lty = 2)
  lines(x = prediction.data$age, y = mu.PI[1,], lty = 2)
  lines(x = prediction.data$age, y = mu.PI[2,], lty = 2)
  shade(object = simulated.heights.PI, lim = prediction.data$age)
}

## plot results
plotResults(model = m1, prediction.data = prediction.data, original.data = d1, n.trials = n.trials)
plotResults(model = m2, prediction.data = prediction.data, original.data = d1, n.trials = n.trials)
plotResults(model = m3, prediction.data = prediction.data, original.data = d1, n.trials = n.trials)
plotResults(model = m4, prediction.data = prediction.data, original.data = d1, n.trials = n.trials)
plotResults(model = m5, prediction.data = prediction.data, original.data = d1, n.trials = n.trials)
plotResults(model = m6, prediction.data = prediction.data, original.data = d1, n.trials = n.trials)

## 6H3

# plot ensembled predictions
comparison <- compare(m1, m2, m3, m4, m5, m6)

# couldn't figure out how to subset the `comparison` object, so just plucked these values manually
weights <- c(.57, .27, .16)

simulated.heights.m4 <- simulateHeights(model = m4, prediction.data = prediction.data)
simulated.heights.m5 <- simulateHeights(model = m5, prediction.data = prediction.data)
simulated.heights.m6 <- simulateHeights(model = m6, prediction.data = prediction.data)

simulated.heights.ensemble <- .57 * simulated.heights.m4 + .27 * simulated.heights.m5 + .16 * simulated.heights.m6

# plot results
plot(height ~ age, data = d1, col = rangi2)
simulated.heights.ensemble.PI <- apply(X = simulated.heights.ensemble, MARGIN = 2, FUN = PI)
shade(object = simulated.heights.ensemble.PI, lim = prediction.data$age)

# author's code
h.ensemble <- ensemble( m4,m5,m6 , data=list(age=age.seq) )
mu.mean <- apply( h.ensemble$link , 2 , mean )
mu.ci <- apply( h.ensemble$link , 2 , PI )
height.ci <- apply( h.ensemble$sim , 2 , PI )

plot( height ~ age , d1 , col="slateblue" , xlim=c(-2,3) )
lines( age.seq , mu.mean )
shade( mu.ci , age.seq )
shade( height.ci , age.seq )
