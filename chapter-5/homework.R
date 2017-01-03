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
