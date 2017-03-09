# homework

library(rethinking)

## 13M1

# define hyperparameters
average.morning.wait.time <- 3.5
average.difference.afternoon.wait.time <- (-1)
sigma.average.morning.wait.time <- 1
sigma.average.difference.afternoon.wait.time <- 0.5
correlation <- (-0.7)

# define likelihood objects
Mu <- c(average.morning.wait.time, average.difference.afternoon.wait.time)
covariance <- sigma.average.morning.wait.time*sigma.average.difference.afternoon.wait.time*correlation
Sigma <- matrix(c(sigma.average.morning.wait.time^2, covariance, covariance, sigma.average.difference.afternoon.wait.time^2), ncol=2)

# simulate varying effects
n.cafes <- 20
library(MASS)
set.seed(5)
vary_effects <- mvrnorm( n = n.cafes , mu = Mu , Sigma = Sigma )
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]

# simulate wait times
n.visits <- 10
afternoon <- rep( 0:1, n.visits*n.cafes/2 )
cafe_id <- rep( 1:n.cafes , each=n.visits )
mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
sigma <- 0.5
wait <- rnorm( n = n.visits*n.cafes, mean = mu, sd = sigma )
d <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )


# build model
a <- average.morning.wait.time
b <- average.difference.afternoon.wait.time

m13M1 <- map2stan(
  alist(
    wait ~ dnorm( mu , sigma ),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    c(a_cafe, b_cafe)[cafe] ~ dmvnorm2(Mu = c(a, b), sigma = sigma_cafe, Rho = Rho),
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 10),
    sigma_cafe ~ dcauchy(0, 2),
    sigma ~ dcauchy(0, 2),
    Rho ~ dlkjcorr(2)
  ),
  data=d ,
  iter=5000 , warmup=2000 , chains=2 )

# sample from posterior
posterior.samples <- extract.samples(m13M1)

# plot posterior for rho
dens( posterior.samples$Rho[,1,2] )

## 13M2
m13M2 <- map2stan(
  alist(
    wait ~ dnorm( mu , sigma ),
    mu <- a_cafe[cafe] + b_cafe[cafe]*afternoon,
    a_cafe[cafe] ~ dnorm(a, sigma_a),
    b_cafe[cafe] ~ dnorm(b, sigma_b),
    a ~ dnorm(0, 10),
    b ~ dnorm(0, 10),
    sigma ~ dcauchy(0, 1),
    sigma_a ~ dcauchy(0, 1),
    sigma_b ~ dcauchy(0, 1)
  ),
  data=d ,
  iter=5000 , warmup=2000 , chains=2 )

# compare models
compare(m13M1, m13M2)

posterior.samples.m13M1 <- extract.samples(m13M1)
a13M1 <- apply( X = posterior.samples.m13M1$a_cafe , MARGIN = 2 , FUN = mean )
b13M1 <- apply( X = posterior.samples.m13M1$b_cafe , MARGIN = 2 , FUN = mean )
posterior.samples.m13M2 <- extract.samples(m13M2)
a13M2 <- apply( X = posterior.samples.m13M2$a_cafe, MARGIN = 2 , FUN = mean )
b13M2 <- apply( X = posterior.samples.m13M2$b_cafe, MARGIN = 2 , FUN = mean )
plot( a13M1 , b13M1 , xlab="intercept" , ylab="slope" ,
      pch=16 , col=rangi2 , ylim=c( min(b13M1)-0.05 , max(b13M1)+0.05 ) ,
      xlim=c( min(a13M1)-0.1 , max(a13M1)+0.1 ) )
points( a13M2 , b13M2 , pch=1 )

# The data-generation process included a negative correlation between intercept and slope. Because the intercepts (x-values) are larger than average
# to the right of the center, the blue points (the ones from the model including correlation) are pushed to be smaller than average on the y-axis.
# Conversely, the x-values to the left of center push the y-values in blue to be larger.

## 13M3
data(UCBadmit)
d <- UCBadmit
d$male <- ifelse( d$applicant.gender=="male" , 1 , 0 )
d$dept_id <- coerce_index( d$dept )

m13M3 <- map2stan(
  alist(
    admit ~ dbinom( applications, p ),
    logit(p) <- a_dept[dept_id] +
      bm_dept[dept_id]*male,
    c(a_dept, bm_dept)[dept_id] ~ dmvnorm2( c(a, bm), sigma_dept, Rho ),
    a ~ dnorm(0, 10),
    bm ~ dnorm(0, 1),
    sigma_dept ~ dcauchy(0, 2),
    Rho ~ dlkjcorr(2)
  ),
  data=d , warmup=1000 , iter=5000 , chains=4 , cores=3 )

m13M3.noncentered <- map2stan(
  alist(
    admit ~ dbinom( applications, p ),
    logit(p) <- a_dept[dept_id] + bm_dept[dept_id]*male,
    c(a_dept, bm_dept)[dept_id] ~ dmvnormNC( sigma_dept, Rho ),
    a ~ dnorm(0, 10),
    bm ~ dnorm(0, 1),
    sigma_dept ~ dcauchy(0, 2),
    Rho ~ dlkjcorr(2)
  ),
  data=d , warmup=1000 , iter=5000 , chains=4 , cores=3 )

# compare centered and non-centered models
compare(m13M3, m13M3.noncentered)

# Models look identical, with the same "flexibility," similar weight, and a "standard error of the difference between WAIC values" greater
# than the difference itself. This said, the non-centered model samples much more efficiently, evidenced by the `n_eff` counts on `a` and `bm`.

## 13H1
data(bangladesh)
d <- bangladesh
d$district_id <- coerce_index(d$district)
d$use_contraception <- d$use.contraception

m13H1 <- map2stan(
  alist(
    use_contraception ~ dbinom(1, p),
    logit(p) <- alpha + alpha_district[district_id] + (beta + beta_district[district_id])*urban,
    c(alpha_district, beta_district)[district_id] ~ dmvnorm2(Mu = 0, sigma = Sigma, Rho = Rho),
    alpha ~ dnorm(0, 10),
    beta ~ dnorm(0, 10),
    Sigma ~ dcauchy(0, 2),
    Rho ~ dlkjcorr(2)
  ),
  data = d, chains = 2, warmup = 1000, iter = 4000
)

# inspect posterior distribution for Rho
posterior.samples <- extract.samples(m13H1)
dens( posterior.samples$Rho[,1,2] )

# inspect estimates
precis(m13H1, pars = c("alpha", "beta"), depth = 2)

# Urban context is of comparatively smaller impact on contraceptive use in districts (within-cluster) that have higher-than-average contraceptive use (in rural areas); urban context
# is of comparatively higher impact on contraceptive use in districts that have lower-than-average contraceptive use (in rural areas).

# take some of the author's plots
pred.dat.rural <- list(
  urban=rep(0,60),
  district_id=1:60 )
pred.dat.urban <- list(
  urban=rep(1,60),
  district_id=1:60 )
pred.rural <- link( m13H1 , data=pred.dat.rural )
pred.urban <- link( m13H1 , data=pred.dat.urban )
means.rural <- apply( pred.rural , 2 , mean )
means.urban <- apply( pred.urban , 2 , mean )
plot( means.rural , means.urban , col="slateblue" ,
      xlim=c(0,1) , ylim=c(0,1) ,
      xlab="rural use" , ylab="urban use" )
abline(a=0,b=1,lty=2)

plot( means.rural , means.urban-means.rural , col="slateblue" ,
      xlab="rural use" , ylab="difference btw urban and rural" )
abline(h=0,lty=2)

## 13H2
data(Oxboys)
d <- Oxboys
d$height_centered <- (d$height - mean(d$height)) / sd(d$height)

m13H2 <- map2stan(
  alist(
    height_centered ~ dnorm(mu, sigma),
    mu <- alpha + alpha_subject[Subject] + (beta + beta_subject[Subject])*age,
    alpha ~ dnorm(0, 10),
    beta ~ dnorm(0, 10),
    c(alpha_subject, beta_subject)[Subject] ~ dmvnormNC(sigma = sigma_subject, Rho = Rho),
    sigma ~ dcauchy(0, 2),
    sigma_subject ~ dcauchy(0, 2),
    Rho ~ dlkjcorr(2)
  ),
  data = d, chains = 2, warmup = 1000, iter = 4000
)

## 13H3

# Boys that are already taller than most, grow faster.
