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