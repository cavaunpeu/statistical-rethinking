# notes

library(rethinking)

## 13.1
average.morning.wait.time <- 3.5
average.difference.afternoon.wait.time <- (-1)
sigma.average.morning.wait.time <- 1
sigma.average.difference.afternoon.wait.time <- 0.5
correlation <- (-0.7)

## 13.2
Mu <- c(average.morning.wait.time, average.difference.afternoon.wait.time)

## 13.3
covariance <- sigma.average.morning.wait.time*sigma.average.difference.afternoon.wait.time*correlation
Sigma <- matrix(c(sigma.average.morning.wait.time^2, covariance, covariance, sigma.average.difference.afternoon.wait.time^2), ncol=2)

## 13.5
sigmas <- c(sigma.average.morning.wait.time, sigma.average.difference.afternoon.wait.time)
Rho <- matrix( c(1, correlation, correlation, 1) , nrow=2 )
Sigma <- diag(sigmas) %*% Rho %*% diag(sigmas)

## 13.6
n.cafes <- 20

## 13.7
library(MASS)
set.seed(5)
vary_effects <- mvrnorm( n = n.cafes , mu = Mu , Sigma = Sigma )

## 13.8
a_cafe <- vary_effects[,1]
b_cafe <- vary_effects[,2]

## 13.9
plot( a_cafe , b_cafe , col=rangi2 ,
      xlab="intercepts (a_cafe)" , ylab="slopes (b_cafe)" )

# overlay population distribution
library(ellipse)
for ( l in c(0.1, 0.3, 0.5, 0.8, 0.99) ) {
  lines(ellipse(Sigma, centre=Mu, level=l), col=col.alpha("black", 0.2))
}

## 13.10
n.visits <- 10
afternoon <- rep( 0:1, n.visits*n.cafes/2 )
cafe_id <- rep( 1:n.cafes , each=n.visits )

mu <- a_cafe[cafe_id] + b_cafe[cafe_id]*afternoon
sigma <- 0.5  # std dev within cafes
wait <- rnorm( n = n.visits*n.cafes, mean = mu, sd = sigma )
d <- data.frame( cafe=cafe_id , afternoon=afternoon , wait=wait )

## 13.11
R <- rlkjcorr( n = 1e4 , K = 2 , eta = 2 )
dens( x = R[,1,2] , xlab = "correlation" )

## 13.12
a <- average.morning.wait.time
b <- average.difference.afternoon.wait.time

m13.1 <- map2stan(
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

## 13.13
posterior.samples <- extract.samples(m13.1)
dens( posterior.samples$Rho[,1,2] )

## 13.14

# compute unpooled estimates directly from data
a1 <- sapply( 1:n.cafes ,
              function(i) mean(wait[cafe_id==i & afternoon==0]) )
b1 <- sapply( 1:n.cafes ,
              function(i) mean(wait[cafe_id==i & afternoon==1]) ) - a1

# extract posterior means of partially pooled estimates
posterior.samples <- extract.samples(m13.1)
a2 <- apply( X = posterior.samples$a_cafe, MARGIN = 2, FUN = mean )
b2 <- apply( X = posterior.samples$b_cafe, MARGIN = 2, FUN = mean )

# plot both and connect with lines
plot( x = a1 , y = b1 , xlab="intercept" , ylab="slope" ,
      pch=16 , col=rangi2 , ylim=c( min(b1)-0.1 , max(b1)+0.1 ) ,
      xlim=c( min(a1)-0.1 , max(a1)+0.1 ) )
points( a2 , b2 , pch=1 )
for ( i in 1:n.cafes ) lines( c(a1[i],a2[i]) , c(b1[i],b2[i]) )
