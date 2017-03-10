# homework

library(rethinking)

## 14H1
data(elephants)
d <- elephants

# fit a Poisson model using age to predict matings
m14H1 <- map2stan(
  alist(
    MATINGS ~ dpois(lambda),
    log(lambda) <- alpha + beta*AGE,
    alpha ~ dnorm(0, 10),
    beta ~ dnorm(0, 1)
  ),
  data = d, iter = 1e4, chains = 2
)

# fit a Poisson model using age to predict matings, yet incorporate standard error
m14H1.se <- map2stan(
  alist(
    MATINGS ~ dpois(lambda),
    log(lambda) <- alpha + beta*AGE_true[i],
    AGE ~ dnorm(AGE_true, 5),
    alpha ~ dnorm(0, 10),
    beta ~ dnorm(0, 1)
  ),
  data = d, iter = 1e4, chains = 2, WAIC = FALSE,
  start = list(AGE_true = d$AGE)
)

# extract link samples
age.seq <- seq(from = 25, to = 55, length.out = 30)
lambda <- link(fit = m14H1, data = list(AGE = age.seq))
lambda.mean <- apply(X = lambda, MARGIN = 2, FUN = mean)
lambda.PI <- apply(X = lambda, MARGIN = 2, FUN = PI)

# plot inferred trend
plot(MATINGS ~ AGE, data = d, col = rangi2, pch = 16)
lines(x = age.seq, y = lambda.mean)
shade(lambda.PI, age.seq)

# visualize the "movement" of values from AGE --> AGE_true
posterior.samples.se <- extract.samples(object = m14H1.se)
AGE_true <- apply(X = posterior.samples.se$AGE_true, MARGIN = 2, FUN = mean)
MATINGS_jitter <- jitter(d$MATINGS)
plot(MATINGS_jitter ~ d$AGE, col = rangi2, pch = 16)
points(AGE_true, MATINGS_jitter)
for ( i in 1:nrow(d) )
  lines( c(d$AGE[i], AGE_true[i]) , rep(MATINGS_jitter[i], 2) )
