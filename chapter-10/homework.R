# homework

## 10M1
# When moving from disaggregated to aggregated counts, the likelihood function must change from a Bernoulli/Binomial based model to a Poisson model 
# for trivial reasons: we are no longer modeling binary events, but instead discrete counts of events.

## 10M2
# A 1-unit increase in the respective predictor x should produce an exp(1.7) = 5.473x increase in the positive probability of the outcome variable 
# in a given (single) trial.

## 10M3
# In a binomial generalized linear model we are typically modeling the `p` parameter with a linear model. Because this parameter is defined as a probability
# mass it must be constrained to the interval [0, 1], and the logit link function ensures this constraint.

## 10M4
# In a Poisson generalized linear model we are typically modeling the `lambda` parameter with a linear model. This parameter must be positive (it gives
# expected value of the random variable, after all), and the log link function ensures this constraint.

## 10M5
# Concretely, using a logit link in a Poisson GLM would imply that the expected value would need to be between 0 and 1. My first instinct is that this has
# no real place, as this would convey a model in which we're after consecutive occurences of the positive event, or something to this effect. A Poisson(.8) curve
# is given here:

curve(dpois(x, .8), from = 0, to = 10)

## 10M6

# When only two event-types are possible and the expected numbers of each type are assumed to be constant across trials (i.e. the probability of
# each event is constant) then the binomial distribution gives the maximum entropy distribution.

# The Poisson distribution is used for counts that never get close to any theoretical maximum. It is the maximum entropy distribution under the same
# constraints as the Binomial distribution.

# The constraints for each are identical, as the latter is simply a special case of the former.

## 10H1
library(rethinking)
data(chimpanzees)
d <- chimpanzees

# fit with map
m10H1 <- map(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left,
    a[actor] ~ dnorm(0, 10),
    bp ~ dnorm(0, 10),
    bpC ~ dnorm(0, 10)
  ),
  data = d
)

# fit with map2stan
d2 <- d
d2$recipient <- NULL

m10H1.stan <- map2stan(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left,
    a[actor] ~ dnorm(0, 10),
    bp ~ dnorm(0, 10),
    bpC ~ dnorm(0, 10)
  ),
  data = d2, chains = 2, iter = 2500, warmup = 500
)

# compare
precis(m10H1, depth = 2)
precis(m10H1.stan, depth = 2)

# The MAP estimation of a GLM assumes symmetric uncertainty around the posterior mode. In a logistic regression, uncertainty around a given parameter -
# `bpC` in this case, for example - should not be assumed to be symmetric, as middling values of this coefficient will produce altogether different
# marginal (proportional) changes in `p` (the probability of the positive event) than will extreme ones.

# In this particular model, the estimates for the intercept for the second chimp differ greatly. This chimp never pulled the righthand lever, implying that
# an infinite number of values for this intercept (given the fundamental behavior of the sigmoid, i.e. "saturation" when the values are really big
# or small) would be valid. MCMC can recover this "long-tail" of the associated posterior, while MAP - which assumes symmetry - cannot.

## 10H2
m10.1 <- map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a ,
    a ~ dnorm(0,10)
  ),
  data=d2 , chains=2 )

m10.2 <- map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a + bp*prosoc_left ,
    a ~ dnorm(0,10) ,
    bp ~ dnorm(0,10)),
  data=d2 , chains=2 )

m10.3 <- map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a + (bp + bpC*condition)*prosoc_left ,
    a ~ dnorm(0,10) ,
    bp ~ dnorm(0,10) ,
    bpC ~ dnorm(0,10)
  ),
  data=d2 , chains=2 )

m10.4 <- map2stan(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left ,
    a[actor] ~ dnorm(0,10),
    bp ~ dnorm(0,10),
    bpC ~ dnorm(0,10)
  ),
  data=d2 , chains=2 , iter=2500 , warmup=500 )

# compare
compare(m10.1,m10.2,m10.3,m10.4)

# Looks like m10.4 - the model with the unique intercepts for each chimp - wins.

## 10H3

# a)

library(MASS)
data(eagles)
d <- eagles
d$V <- ifelse(test = d$V == "L", yes = 1, no = 0)
d$A <- ifelse(test = d$A == "A", yes = 1, no = 0)
d$P <- ifelse(test = d$P == "L", yes = 1, no = 0)

m10H3.stan <- map2stan(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- alpha + beta_P*P + beta_A*A + beta_V*V,
    alpha ~ dnorm(0, 10),
    beta_P ~ dnorm(0, 5),
    beta_A ~ dnorm(0, 5),
    beta_V ~ dnorm(0, 5)
  ),
  data = d, warmup = 500, iter = 2500, chains = 2
)

m10H3.map <- map(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- alpha + beta_P*P + beta_A*A + beta_V*V,
    alpha ~ dnorm(0, 10),
    beta_P ~ dnorm(0, 5),
    beta_A ~ dnorm(0, 5),
    beta_V ~ dnorm(0, 5)
  ),
  data = d
)

# compare
precis(m10H3.stan)
precis(m10H3.map)

pairs(m10H3.stan)
pairs(m10H3.map)

# When looking at the pairs plot for the model fit with MCMC, we can see the same long-tail effect in both beta_P and beta_V as we saw in 10H1. 
# In the model fit with MAP, we see symmetric posteriors for these parameters. The former is what we'd expect to see, as our logistic functions allows
# for a long tail of valid parameters for a given outcome.

# b)

# Just looking at the MAP values for each of the parameters:

# V: A value of "large" (i.e. the variable "V" is "on") would cause a sigmoid(-5.04) = 0.0064% increase in the probability of the positive event.
# P: A value of "large" would cause a sigmoid(4.64) = 0.9904% increase in the probability of the positive event.
# A: A value of "adult" would cause a sigmoid(4.64) = 0.753% increase in the probability of the positive event.
#
# Of course, in real-value terms (the actual value of `p`), these marginal changes all depends on what the values of the other variables are.

p <- link(m10H3.stan)
y <- sim(m10H3.stan)

p.mean <- apply(X = p, MARGIN = 2, FUN = mean)
p.PI <- apply(X = p, MARGIN = 2, FUN = PI)
y.mean <- apply(X = y, MARGIN = 2, FUN = mean)
y.PI <- apply(X = y, MARGIN = 2, FUN = PI)

# plot the model predictions for `p` vs. the actual proportion of successes for each case
d$success.proportion <- d$y / d$n
plot(d$success.proportion, col=rangi2, ylab="successful proportion", xlab="case", xaxt="n", xlim=c(0.75,8.25) , ylim = c(0, 1), pch=16)
axis(1, at=1:8, labels=c( "LAL","LAS","LIL","LIS","SAL","SAS","SIL","SIS" ))
points( 1:8 , p.mean )
for ( i in 1:8 ) lines( c(i, i), p.PI[,i] )

# plot the model predictions for `y` vs. the actual number of successes for each case
plot(d$y, col=rangi2, ylab="number of successes successful", xlab="case", xaxt="n", xlim=c(0.75,8.25) , ylim = c(0, 30), pch=16)
axis(1, at=1:8, labels=c( "LAL","LAS","LIL","LIS","SAL","SAS","SIL","SIS" ))
points( 1:8 , y.mean )
for ( i in 1:8 ) lines( c(i, i), y.PI[,i] )

# c)

m10H3.stan.interaction <- map2stan(
  alist(
    y ~ dbinom(n, p),
    logit(p) <- alpha + beta_P*P + beta_A*A + beta_V*V + beta_P_A*P*A,
    alpha ~ dnorm(0, 10),
    beta_P ~ dnorm(0, 5),
    beta_A ~ dnorm(0, 5),
    beta_V ~ dnorm(0, 5),
    beta_P_A ~ dnorm(0, 5)
  ),
  data = d, warmup = 500, iter = 2500, chains = 2
)

compare(m10H3.stan, m10H3.stan.interaction)
precis(m10H3.stan.interaction)