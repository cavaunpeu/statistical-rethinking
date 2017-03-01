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

## 11H2

# fit model
model.11H2 <- map2stan(
  alist(
    deaths ~ dgampois( mu , scale ),
    log(mu) <- alpha + beta_femininity*femininity,
    alpha ~ dnorm(0, 10),
    beta_femininity ~ dnorm(0, 10),
    scale ~ dcauchy(0, 2)
  ),
  data=d,
  constraints=list(scale="lower=0"),
  start=list(scale=2) )

# inspect estimates
precis(model.11H2)

# why did the association between `beta_femininity` and `deaths` diminish in strength?

# I assume this is because a much larger range of values of `(alpha, beta_femininity)` can produce, through the Gamma distribution, the same expected
# values `lambda` via the variance in the Gamma process itself.

## 11H3

# standardize predictors
d$damage_norm_std <- (d$damage_norm - mean(d$damage_norm)) / sd(d$damage_norm)
d$femininity_std <- (d$femininity - mean(d$femininity)) / sd(d$femininity)
d$min_pressure_std <- (d$min_pressure - mean(d$min_pressure)) / sd(d$min_pressure)

# fit a series of models with varying interactions
model.11H1.single.interaction.damage <- map2stan(
  alist(
    deaths ~ dpois( lambda ),
    log(lambda) <- alpha + beta_femininity_damage_norm*femininity_std*damage_norm_std,
    alpha ~ dnorm(0, 10),
    beta_femininity_damage_norm ~ dnorm(0, 1)
  ),
  data = d, chains = 2, warmup = 1000, iter = 4000
)

model.11H1.single.interaction.pressure <- map2stan(
  alist(
    deaths ~ dpois( lambda ),
    log(lambda) <- alpha + beta_femininity_min_pressure*femininity_std*min_pressure_std,
    alpha ~ dnorm(0, 10),
    beta_femininity_min_pressure ~ dnorm(0, 1)
  ),
  data = d, chains = 2, warmup = 1000, iter = 4000
)

model.11H1.two.interactions <- map2stan(
  alist(
    deaths ~ dpois( lambda ),
    log(lambda) <- alpha + beta_femininity_min_pressure*femininity_std*min_pressure_std + beta_femininity_damage_norm*femininity_std*damage_norm_std,
    alpha ~ dnorm(0, 10),
    c(beta_femininity_min_pressure, beta_femininity_damage_norm) ~ dnorm(0, 10)
  ),
  data = d, chains = 2, warmup = 1000, iter = 4000
)

# compare models
compare(model.11H1.single.interaction.damage, model.11H1.single.interaction.pressure, model.11H1.two.interactions)
coeftab(model.11H1.single.interaction.damage, model.11H1.single.interaction.pressure, model.11H1.two.interactions)

# plot counterfactuals
damage.sequence <- seq(from = -1, to = 6, length.out = 30)

# "masculine" storms
prediction.data.male <- data.frame(
  femininity_std = -1,
  damage_norm_std = damage.sequence
)
lambda.male <- link(model.11H1.single.interaction.damage, data = prediction.data.male)
lambda.male.mu <- apply(X = lambda.male, MARGIN = 2, FUN = mean)
lambda.male.PI <- apply(X = lambda.male, MARGIN = 2, FUN = PI)

# "feminine" storms
prediction.data.female <- data.frame(
  femininity_std = 1,
  damage_norm_std = damage.sequence
)
lambda.female <- link(model.11H1.single.interaction.damage, data = prediction.data.female)
lambda.female.mu <- apply(X = lambda.female, MARGIN = 2, FUN = mean)
lambda.female.PI <- apply(X = lambda.female, MARGIN = 2, FUN = PI)

# plot (on square root scale, per author's recommendation)
plot(d$damage_norm_std, sqrt(d$deaths), pch=ifelse(d$femininity_std>0,16,1), col=rangi2, xlab="damage_norm", ylab="sqrt(deaths)")
lines( damage.sequence , sqrt(lambda.male.mu) , lty=2 )
shade( sqrt(lambda.male.PI) , damage.sequence )
lines( damage.sequence , sqrt(lambda.female.mu) , lty=1 )
shade( sqrt(lambda.female.PI) , damage.sequence )

## 11H6
library(rethinking)
data(Fish)
d <- Fish

d$persons_std <- (d$persons - mean(d$persons)) / sd(d$persons)
d$hours_std <- (d$hours - mean(d$hours)) / sd(d$hours)
d$loghours <- log(d$hours)

m11.6 <- map2stan(
  alist(
    fish_caught ~ dzipois(p, lambda),
    logit(p) <- alpha_p + beta_camper*camper,
    log(lambda) <- loghours + alpha_lambda + beta_livebait*livebait + beta_persons*persons_std,
    c(alpha_p, alpha_lambda) ~ dnorm(0, 10),
    c(beta_livebait, beta_persons, beta_camper) ~ dnorm(0, 10)
  ), data = d, chains = 4, warmup = 1000, iter = 4000)

# inspect model
precis(m11.6)

# create counterfactual predictions
prediction.data <- list(
  loghours = log(1),
  camper = 0,
  persons_std = 1,
  livebait = 1
)

fish.link <- link(fit = m11.6, data = prediction.data)
p <- fish.link$p
lambda <- fish.link$lambda

expected.fish.mean <- mean( (1 - p)*lambda )
expected.fish.PI <- PI( (1 - p)*lambda )

expected.fish.mean
expected.fish.PI
