# notes

## 11.1
library(rethinking)
data(Trolley)
d <- Trolley

## 11.2
simplehist( d$response , xlim=c(1,7) , xlab="response" )

## 11.3

# discrete proportion of each response value
pr_k <- table( d$response ) / nrow(d)

# cumsum converts to cumulative proportions
cum_pr_k <- cumsum( pr_k )

# plot
plot( 1:7 , cum_pr_k , type="b" , xlab="response" ,
      ylab="cumulative proportion" , ylim=c(0,1) )

## 11.4
logit <- function(x) log( x/ (1-x) )
lco <- logit( cum_pr_k )

## 11.5
m11.1 <- map(
  alist(
    response ~ dordlogit( phi , c(a1,a2,a3,a4,a5,a6) ),
    phi <- 0,
    c(a1,a2,a3,a4,a5,a6) ~ dnorm(0,10)
  ),
  data=d , start=list(a1=-2,a2=-1,a3=0,a4=1,a5=2,a6=2.5) )

## 11.6
precis(m11.1)

## 11.7
logistic(coef(m11.1))

## 11.8

# note that data with name 'case' not allowed in Stan
# so will pass pruned data list
m11.1stan <- map2stan(
  alist(
    response ~ dordlogit( phi , cutpoints ),
    phi <- 0,
    cutpoints ~ dnorm(0,10)
  ),
  data=list(response=d$response), start=list(cutpoints=c(-2,-1,0,1,2,2.5)) , chains=2 , cores=2 )

# need depth=2 to show vector of parameters
precis(m11.1stan,depth=2)

## 11.9
pk <- dordlogit( 1:7 , 0 , coef(m11.1) )

## 11.10
sum( pk*(1:7) )

## 11.11
pk <- dordlogit( 1:7 , 0 , coef(m11.1)-0.5 )

## 11.12
sum( pk*(1:7) )

## 11.13
m11.2 <- map(
  alist(
    response ~ dordlogit( phi, c(a1, a2, a3, a4, a5, a6) ) ,
    phi <- bA*action + bI*intention + bC*contact,
    c(bA, bI, bC) ~ dnorm(0, 10),
    c(a1, a2, a3, a4, a5, a6) ~ dnorm(0, 10) ),
  data=d ,
  start=list(a1=-1.9, a2=-1.2, a3=-0.7, a4=0.2, a5=0.9, a6=1.8) )

## 11.14
m11.3 <- map(
  alist(
    response ~ dordlogit(phi , c(a1, a2, a3, a4, a5, a6)) ,
    phi <- bA*action + bI*intention + bC*contact +
      bAI*action*intention + bCI*contact*intention ,
    c(bA, bI, bC, bAI, bCI) ~ dnorm(0, 10),
    c(a1, a2, a3, a4, a5, a6) ~ dnorm(0, 10)
  ),
  data=d ,  start=list(a1=-1.9, a2=-1.2, a3=-0.7, a4=0.2, a5=0.9, a6=1.8) )

## 11.15
coeftab(m11.1,m11.2,m11.3)

## 11.16
compare( m11.1 , m11.2 , m11.3 , refresh=0.1 )

## 11.17
post <- extract.samples( m11.3 )

## 11.18
plot( 1 , 1 , type="n" , xlab="intention" , ylab="probability" ,
      xlim=c(0,1) , ylim=c(0,1) , xaxp=c(0,1,1) , yaxp=c(0,1,2) )

## 11.19
kA <- 0 # value for action
kC <- 1 # value for contact
kI <- 0:1 # values of intention to calculate over
for ( s in 1:100 ) {
  p <- post[s,]
  ak <- as.numeric(p[1:6])
  phi <- p$bA*kA + p$bI*kI + p$bC*kC +
    p$bAI*kA*kI + p$bCI*kC*kI
  pk <- pordlogit( 1:6 , a=ak , phi=phi )
  for ( i in 1:6 )
    lines( kI , pk[,i] , col=col.alpha(rangi2,0.1) )
  mtext( concat( "action=",kA,", contact=",kC ) )
}

## 11.20

# define parameters
prob_drink <- 0.2 # 20% of days
rate_work <- 1    # average 1 manuscript per day

# sample one year of production
N <- 365

# simulate days monks drink
drink <- rbinom( N , 1 , prob_drink )

# simulate manuscripts completed
y <- (1-drink)*rpois( N , rate_work )

## 11.21
simplehist( y , xlab="manuscripts completed" , lwd=4 )
zeros_drink <- sum(drink)
zeros_work <- sum(y==0 & drink==0)
zeros_total <- sum(y==0)
lines( c(0,0) , c(zeros_work,zeros_total) , lwd=4 , col=rangi2 )

## 11.22
m11.4 <- map(
  alist(
    y ~ dzipois( p , lambda ),
    logit(p) <- ap,
    log(lambda) <- al,
    ap ~ dnorm(0,1),
    al ~ dnorm(0,10) ),
  data=list(y=y) )
precis(m11.4)

## 11.25
pbar <- 0.5
theta <- 10
curve( dbeta2(x,pbar,theta) , from=0 , to=1 ,
       xlab="probability" , ylab="Density" )

## 11.26
library(rethinking)
data(UCBadmit)
d <- UCBadmit

m11.5 <- map2stan(
  alist(
    admit ~ dbetabinom(applications, pbar, theta),
    logit(pbar) <- a,
    a ~ dnorm(0,2),
    theta ~ dexp(1)
  ),
  data=d,
  constraints=list(theta="lower=0"),
  start=list(theta=3),
  iter=4000 , warmup=1000 , chains=2 , cores=2 )

## 11.27
precis(m11.5)

## 11.28
post <- extract.samples(m11.5)
quantile( logistic(post$a) , c(0.025,0.5,0.975) )

## 11.29
post <- extract.samples(m11.5)

# draw posterior mean beta distribution
curve( dbeta2(x,mean(logistic(post$a)),mean(post$theta)) , from=0 , to=1 ,
       ylab="Density" , xlab="probability admit", ylim=c(0,3) , lwd=2 )

# draw 100 beta distributions sampled from posterior
for ( i in 1:100 ) {
  p <- logistic( post$a[i] )
  theta <- post$theta[i]
  curve( dbeta2(x,p,theta) , add=TRUE , col=col.alpha("black",0.2) )
}

## 11.30
postcheck(m11.5)

## 11.31
mu <- 20
theta <- 10
curve( dgamma2(x,mu,theta) , from=0 , to=10 )
