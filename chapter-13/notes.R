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

## 13.15

# compute posterior mean bivariate Gaussian
Mu_est <- c( mean(posterior.samples$a) , mean(posterior.samples$b) )
rho_est <- mean( posterior.samples$Rho[,1,2] )
sa_est <- mean( posterior.samples$sigma_cafe[,1] )
sb_est <- mean( posterior.samples$sigma_cafe[,2] )
cov_ab <- sa_est*sb_est*rho_est
Sigma_est <- matrix( c(sa_est^2, cov_ab, cov_ab, sb_est^2) , ncol=2 )

# draw contours
for ( l in c(0.1,0.3,0.5,0.8,0.99) ) {
  lines(ellipse(Sigma_est, centre=Mu_est, level=l),
        col=col.alpha("black",0.2))
}

## 13.17
data(UCBadmit)
d <- UCBadmit
d$male <- ifelse( d$applicant.gender=="male" , 1 , 0 )
d$dept_id <- coerce_index( d$dept )

## 13.18
m13.2 <- map2stan(
  alist(
    admit ~ dbinom( applications , p ),
    logit(p) <- a_dept[dept_id] + bm*male,
    a_dept[dept_id] ~ dnorm( a , sigma_dept ),
    a ~ dnorm(0,10),
    bm ~ dnorm(0,1),
    sigma_dept ~ dcauchy(0,2)
  ),
  data=d , warmup=500 , iter=4500 , chains=3 )
precis( m13.2 , depth=2 )

## 13.19
m13.3 <- map2stan(
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

## 13.22
data(chimpanzees)
d <- chimpanzees
d$recipient <- NULL
d$block_id <- d$block

m13.6 <- map2stan(
  alist(
    pulled_left ~ dbinom(1,p),
    logit(p) <- A + (BP + BPC*condition)*prosoc_left,
    A <- a + a_actor[actor] + a_block[block_id],
    BP <- bp + bp_actor[actor] + bp_block[block_id],
    BPC <- bpc + bpc_actor[actor] + bpc_block[block_id],
    c(a_actor, bp_actor, bpc_actor)[actor] ~ dmvnorm2(0, sigma_actor, Rho_actor),
    c(a_block, bp_block, bpc_block)[block_id] ~ dmvnorm2(0, sigma_block, Rho_block),
    c(a, bp, bpc) ~ dnorm(0, 1),
    sigma_actor ~ dcauchy(0, 2),
    sigma_block ~ dcauchy(0, 2),
    Rho_actor ~ dlkjcorr(4),
    Rho_block ~ dlkjcorr(4)
  ) , data=d , iter=5000 , warmup=1000 , chains=3 , cores=3 )

## 13.23
m13.6NC <- map2stan(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- A + (BP + BPC*condition)*prosoc_left,
    A <- a + a_actor[actor] + a_block[block_id],
    BP <- bp + bp_actor[actor] + bp_block[block_id],
    BPC <- bpc + bpc_actor[actor] + bpc_block[block_id],
    c(a_actor, bp_actor, bpc_actor)[actor] ~
      dmvnormNC(sigma_actor, Rho_actor),
    c(a_block, bp_block, bpc_block)[block_id] ~
      dmvnormNC(sigma_block, Rho_block),
    c(a,bp,bpc) ~ dnorm(0, 1),
    sigma_actor ~ dcauchy(0, 2),
    sigma_block ~ dcauchy(0, 2),
    Rho_actor ~ dlkjcorr(4),
    Rho_block ~ dlkjcorr(4)
  ) , data=d , iter=5000 , warmup=1000 , chains=3 , cores=3 )
precis(m13.6NC, depth = 2)

## 13.25
precis( m13.6NC , depth=2 , pars=c("sigma_actor", "sigma_block") )

## 13.29
data(islandsDistMatrix)
Dmat <- islandsDistMatrix
colnames(Dmat) <- c("Ml", "Ti", "SC", "Ya", "Fi", "Tr", "Ch", "Mn", "To", "Ha")
round(Dmat, 1)
