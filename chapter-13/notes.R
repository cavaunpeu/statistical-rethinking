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
