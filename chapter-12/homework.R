# homework

## 12M1
library(rethinking)
data(reedfrogs)
d <- reedfrogs
str(d)

# make the tank cluster variable
d$tank <- 1:nrow(d)
d$predation <- ifelse(test = d$pred == "pred", yes = 1, no = 0)
d$frogsize <- ifelse(test = d$size == "big", yes = 1, no = 0)

# fit several models
m12M1.predation <- map2stan(
  alist(
    surv ~ dbinom(density, p) ,
    logit(p) <- alpha[tank] + beta_predation*predation,
    alpha[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 1),
    beta_predation ~ dnorm(0, 1)
  ), data=d )

m12M1.size <- map2stan(
  alist(
    surv ~ dbinom(density, p) ,
    logit(p) <- alpha[tank] + beta_frogsize*frogsize,
    alpha[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 1),
    beta_frogsize ~ dnorm(0, 1)
  ), data=d )

m12M1.both <- map2stan(
  alist(
    surv ~ dbinom(density, p) ,
    logit(p) <- alpha[tank] + beta_predation*predation + beta_frogsize*frogsize,
    alpha[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 1),
    c(beta_predation, beta_frogsize) ~ dnorm(0, 1)
  ), data=d )

m12M1.interaction <- map2stan(
  alist(
    surv ~ dbinom(density, p) ,
    logit(p) <- alpha[tank] + beta_predation*predation + beta_frogsize*frogsize + beta_predation_frogsize*predation*frogsize,
    alpha[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 1),
    c(beta_predation, beta_frogsize, beta_predation_frogsize) ~ dnorm(0, 1)
  ), data=d )

# posterior predictive check
ppc <- function(model, df) {
  post <- extract.samples(model)

  # compute median intercept for each tank
  # also transform to probability with logistic
  df$propsurv.est <- logistic( apply( X = post$alpha, MARGIN = 2, FUN = median ) )

  # display raw proportions surviving in each tank
  plot( df$propsurv , ylim=c(0,1) , pch=16 , xaxt="n" ,
        xlab="tank" , ylab="proportion survival" , col=rangi2 )
  axis( 1 , at=c(1,16,32,48) , labels=c(1,16,32,48) )

  # overlay posterior medians
  points( df$propsurv.est )

  # mark posterior median probability across tanks
  abline( h=logistic(median(post$alpha)) , lty=2 )

  # draw vertical dividers between tank densities
  abline( v=16.5 , lwd=0.5 )
  abline( v=32.5 , lwd=0.5 )
  text( 8 , 0 , "small tanks" )
  text( 16+8 , 0 , "medium tanks" )
  text( 32+8 , 0 , "large tanks" )
}

ppc(model = m12M1.predation, df = d)
ppc(model = m12M1.size, df = d)
ppc(model = m12M1.both, df = d)
ppc(model = m12M1.interaction, df = d)

coeftab(m12M1.predation, m12M1.size, m12M1.both, m12M1.interaction)

# As we add more predictor variables, the variation amongst tanks decreases. This is because predictor variables, by definition,
# "explain away" the variance, leaving less to be captured by `sigma` itself.

## 12M2
compare(m12M1.predation, m12M1.size, m12M1.both, m12M1.interaction)
precis(m12M1.size)
precis(m12M1.predation)

## 12M3
m12M3.alpha.cauchy <- map2stan(
  alist(
    surv ~ dbinom(density, p) ,
    logit(p) <- alpha[tank],
    alpha[tank] ~ dcauchy(m, shape),
    m ~ dnorm(0, 10),
    shape ~ dcauchy(0, 1)
  ), data=d )

m12M3.alpha.normal <- map2stan(
  alist(
    surv ~ dbinom(density, p) ,
    logit(p) <- alpha[tank],
    alpha[tank] ~ dnorm(a, sigma),
    a ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 1)
  ), data=d )

# compare
coeftab(m12M3.alpha.cauchy, m12M3.alpha.normal)

# plot with author's code
post_normal <- extract.samples(m12M3.alpha.normal)
alpha_tank_normal <- apply(X = post_normal$alpha, MARGIN = 2, FUN = mean)
post_cauchy <- extract.samples(m12M3.alpha.cauchy)
alpha_tank_cauchy <- apply(X = post_cauchy$alpha, MARGIN = 2, FUN = mean)
plot( alpha_tank_normal , alpha_tank_cauchy , pch=16 , col=rangi2 ,
      xlab="Gaussian prior" , ylab="Cauchy prior" )
abline(a=0, b=1, lty=2)

# The Cauchy means are bigger in a few cases, as Cauchy priors have the longer tail and therefore more plausible values. As such, while the shrinkage
# of the model does pull these extreme values (all tadpoles survived in these tanks, i.e. a survival proportion of 100%) in from infinity,
# the Gaussian prior does this moreso than the Cauchy.