# homework

## 11M1
ratings <- c(12, 36, 7, 41)
ratings.proportions <- ratings / sum(ratings)
ratings.proportions.cdf <- cumsum(ratings.proportions)
log.cumulative.odds <- log(ratings.proportions.cdf / (1 - ratings.proportions.cdf))

## 11M3
# I believe this one is pretty straightforward. The only thing we must do is swap the Poisson likelihood (which accepts a lambda) with the Binomial
# likelihood (which accepts both a probability of success `p` and the number of trials `N`). As such, our new likelihood of a non-zero value is 
# given by: (1 - p_not_work)Pr(y | N, p).

## 11H1
library(rethinking)
data(Hurricanes)
d <- Hurricanes

# fit Poisson regression using `femininity` as a predictor
model.11H1 <- map2stan(
  alist(
    deaths ~ dpois( lambda ),
    log(lambda) <- alpha + beta_femininity*femininity,
    alpha ~ dnorm(0, 10),
    beta_femininity ~ dnorm(0, 10)
  ),
  data = d, chains = 2, warmup = 1000, iter = 4000
)

# fit Poisson regression using intercept only
model.11H.intercept.only <- map2stan(
  alist(
    deaths ~ dpois( lambda ),
    log(lambda) <- alpha,
    alpha ~ dnorm(0, 10)
  ),
  data = d, chains = 2, warmup = 1000, iter = 4000
)

# compare models
compare(model.11H1, model.11H.intercept.only)

# plot posterior predictions
prediction.data <- list(femininity = seq(from = 1, to = 11, length.out = 30))

# simulate lambda values
lambda <- link(fit = model.11H1, data = prediction.data)
lambda.mean <- apply(X = lambda, MARGIN = 2, FUN = mean)
lambda.PI <- apply(X = lambda, MARGIN = 2, FUN = PI)

# simulate Poisson predictions
simulated.predictions <- sim(fit = model.11H1, data = prediction.data)
simulated.predictions.PI <- apply(X = simulated.predictions, MARGIN = 2, FUN = PI)

plot( d$femininity, d$deaths, pch=16, col=rangi2, xlab="femininity", ylab="deaths" )
lines(x = prediction.data$femininity, y = lambda.mean)
shade(object = lambda.PI, lim = prediction.data$femininity)
lines(x = prediction.data$femininity, y = simulated.predictions.PI[1,], lty = 2)
lines(x = prediction.data$femininity, y = simulated.predictions.PI[2,], lty = 2)
