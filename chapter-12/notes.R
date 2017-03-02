# notes

## 12.1
library(rethinking)
data(reedfrogs)
d <- reedfrogs
str(d)

## 12.2

# make the tank cluster variable
d$tank <- 1:nrow(d)

# fit
m12.1 <- map2stan(
  alist(
    surv ~ dbinom(density, p) ,
    logit(p) <- alpha[tank],
    alpha[tank] ~ dnorm(0, 5)
  ), data=d )

# inspect
precis(m12.1, depth = 2)

## 12.3

m12.2 <- map2stan(
  alist(
    surv ~ dbinom(density, p) ,
    logit(p) <- alpha_tank[tank],
    alpha_tank[tank] ~ dnorm(alpha, sigma),
    alpha ~ dnorm(0, 1),
    sigma ~ dcauchy(0, 1)
  ), data=d )

## 12.4
compare(m12.1, m12.2)

## 12.5

# extract Stan samples
post <- extract.samples(m12.2)

# compute median intercept for each tank
# also transform to probability with logistic
d$propsurv.est <- logistic( apply( X = post$alpha_tank, MARGIN = 2, FUN = median ) )

# display raw proportions surviving in each tank
plot( d$propsurv , ylim=c(0,1) , pch=16 , xaxt="n" ,
      xlab="tank" , ylab="proportion survival" , col=rangi2 )
axis( 1 , at=c(1,16,32,48) , labels=c(1,16,32,48) )

# overlay posterior medians
points( d$propsurv.est )

# mark posterior median probability across tanks
abline( h=logistic(median(post$alpha)) , lty=2 )

# draw vertical dividers between tank densities
abline( v=16.5 , lwd=0.5 )
abline( v=32.5 , lwd=0.5 )
text( 8 , 0 , "small tanks" )
text( 16+8 , 0 , "medium tanks" )
text( 32+8 , 0 , "large tanks" )
