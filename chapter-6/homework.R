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