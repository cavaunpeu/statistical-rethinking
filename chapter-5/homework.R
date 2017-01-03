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
