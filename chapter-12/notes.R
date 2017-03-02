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

## 12.6

# show first 100 populations in the posterior
plot( NULL , xlim=c(-3,4) , ylim=c(0,0.35) ,
      xlab="log-odds survive" , ylab="Density" )

for ( i in 1:100 ) {
  curve( dnorm(x, post$alpha[i], post$sigma[i]), add=TRUE, col=col.alpha("black",0.2) )
}

# sample 8000 imaginary tanks from the posterior distribution
sim_tanks <- rnorm( 8000 , post$alpha , post$sigma )

# transform to probability and visualize
dens( logistic(sim_tanks) , xlab="probability survive" )

## 12.7

# simulate some tadpole survival data

# define parameters
alpha <- 1.4
sigma <- 1.5
n.ponds <- 60
density.of.pond <- as.integer( rep( c(5,10,25,35) , each=15 ) )

# simulate our vector of log-odds of survival intercepts (one for each pond)
alpha_pond <- rnorm( n.ponds , mean=alpha , sd=sigma )
simulated.df <- data.frame( pond=1:n.ponds , density.of.pond=density.of.pond , true_alpha=alpha_pond )
simulated.df$survivors.in.pond <- rbinom( n = n.ponds , prob=logistic(simulated.df$true_alpha) , size=simulated.df$density.of.pond )

# compute the no-pooling estimates
simulated.df$survival.proportion.no.pooling <- simulated.df$survivors.in.pond / simulated.df$density.of.pond

## 12.13
m12.3 <- map2stan(
  alist(
    survivors.in.pond ~ dbinom( density.of.pond , p ),
    logit(p) <- alpha_pond[pond],
    alpha_pond[pond] ~ dnorm( alpha, sigma ),
    alpha ~ dnorm(0,1),
    sigma ~ dcauchy(0,1)
  ),
  data=simulated.df , iter=1e4 , warmup=1000
)

## 12.14
precis(m12.3, depth = 2)

## 12.15
estimated.alpha.pond <- coef(m12.3)[1:60]
simulated.df$survival.proportion.partial.pooling <- logistic(estimated.alpha.pond)

## 12.16
simulated.df$true.survival.proportions.used.to.generate.data <- logistic(simulated.df$true_alpha)

## 12.17
no.pooling.error <- abs(simulated.df$survival.proportion.no.pooling - simulated.df$true.survival.proportions.used.to.generate.data)
partial.pooling.error <- abs(simulated.df$survival.proportion.partial.pooling - simulated.df$true.survival.proportions.used.to.generate.data)

## 12.18
plot( 1:60, no.pooling.error, xlab="pond", ylab="absolute error", col=rangi2, pch=16 )
points( 1:60, partial.pooling.error )

## 12.21
library(rethinking)
data(chimpanzees)
d <- chimpanzees
d$recipient <- NULL # get rid of NAs

m12.4 <- map2stan(
  alist(
    pulled_left ~ dbinom(1, p) ,
    logit(p) <- a + a_actor[actor] + (bp + bpC*condition)*prosoc_left ,
    a_actor[actor] ~ dnorm(0, sigma_actor),
    a ~ dnorm(0, 10),
    bp ~ dnorm(0, 10),
    bpC ~ dnorm(0, 10),
    sigma_actor ~ dcauchy(0, 1)
  ),
  data=d , warmup=1000 , iter=5000 , chains=4 , cores=3 )

## 12.22
posterior.samples <- extract.samples(m12.4)
total_a_actor <- sapply( 1:7 , function(actor) posterior.samples$a + posterior.samples$a_actor[,actor] )
round(apply(X = total_a_actor, MARGIN = 2, FUN = mean) , 2)

## 12.23
d$block_id <- d$block

m12.5 <- map2stan(
  alist(
    pulled_left ~ dbinom(1, p) ,
    logit(p) <- a + a_actor[actor] + a_block[block_id] + (bp + bpC*condition)*prosoc_left ,
    a_actor[actor] ~ dnorm(0, sigma_actor),
    a_block[block_id] ~ dnorm(0, sigma_block),
    c(a, bp, bpC) ~ dnorm(0, 10),
    c(sigma_actor, sigma_block) ~ dcauchy(0, 1)
  ),
  data=d , warmup=1000 , iter=5000 , chains=4 , cores=3 )

## 12.24
precis(m12.5, depth = 2)

## 12.25
posterior.samples <- extract.samples(m12.5)
dens( posterior.samples$sigma_block , xlab="sigma" , xlim=c(0,4) )
dens( posterior.samples$sigma_actor , col=rangi2 , lwd=2 , add=TRUE )
text( 2 , 0.85 , "actor" , col=rangi2 )
text( 0.75 , 2 , "block" )

## 12.26
compare(m12.4, m12.5)

## 12.27

# simulate probability values using the `link` function
chimp <- 2
d.pred <- list(
  prosoc_left = c(0, 1, 0, 1),
  condition = c(0, 0, 1, 1),
  actor = rep(chimp, 4)
)
link.m12.4 <- link( m12.4 , data=d.pred )
pred.p <- apply( link.m12.4 , 2 , mean )
pred.p.PI <- apply( link.m12.4 , 2 , PI )

## 12.28

# simulate (the same) probability values from scratch
posterior.samples <- extract.samples(m12.4)

## 12.29
dens(posterior.samples$a_actor[,5])

## 12.30
probability.link <- function( prosoc_left , condition , actor ) {
  logodds <- with(posterior.samples, a + a_actor[,actor] + (bp + bpC * condition) * prosoc_left)
  return( logistic(logodds) )
}

## 12.31
prosoc_left <- c(0, 1, 0, 1)
condition <- c(0, 0, 1, 1)
pred.raw <- sapply( 1:4 , function(i) probability.link(prosoc_left = prosoc_left[i], condition = condition[i], actor = 2) )
pred.p <- apply( pred.raw , 2 , mean )
pred.p.PI <- apply( pred.raw , 2 , PI )
