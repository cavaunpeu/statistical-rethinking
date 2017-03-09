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
