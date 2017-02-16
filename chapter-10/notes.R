# notes

## 10.1
library(rethinking)
data(chimpanzees)
d <- chimpanzees

## 10.2
m10.1 <- map(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a ,
    a ~ dnorm(0,10)
  ),
  data=d )
precis(m10.1)

## 10.4
m10.2 <- map(
  alist(
    pulled_left ~ dbinom(1, p) ,
    logit(p) <- a + bp*prosoc_left ,
    a ~ dnorm(0,10) ,
    bp ~ dnorm(0,10)
  ),
  data=d )

m10.3 <- map(
  alist(
    pulled_left ~ dbinom(1, p) ,
    logit(p) <- a + (bp + bpC*condition)*prosoc_left ,
    a ~ dnorm(0,10) ,
    bp ~ dnorm(0,10) ,
    bpC ~ dnorm(0,10)
  ), data=d )

## 10.5
compare(m10.1, m10.2, m10.3)

## 10.6
precis(m10.3)

## 10.10

# dummy data for predictions across treatments
d.pred <- data.frame(
  prosoc_left = c(0,1,0,1),   # right/left/right/left
  condition = c(0,0,1,1)      # control/control/partner/partner
)

# build prediction ensemble
chimp.ensemble <- ensemble( m10.1 , m10.2 , m10.3 , data=d.pred )

# summarize
pred.p <- apply( chimp.ensemble$link , 2 , mean )
pred.p.PI <- apply( chimp.ensemble$link , 2 , PI )

## 10.11

# empty plot frame with good axes
plot( 0 , 0 , type="n" , xlab="prosoc_left/condition" ,
      ylab="proportion pulled left" , ylim=c(0,1) , xaxt="n" ,
      xlim=c(1,4) )
axis( 1 , at=1:4 , labels=c("0/0","1/0","0/1","1/1") )

# plot raw data, one trend for each of 7 individual chimpanzees
p <- by( d$pulled_left ,
         list(d$prosoc_left,d$condition,d$actor) , mean )
for ( chimp in 1:7 )
  lines( 1:4 , as.vector(p[,,chimp]) , col=rangi2 , lwd=1.5 )

# superimpose posterior predictions
lines( 1:4 , pred.p )
shade( pred.p.PI , 1:4 )

## 10.12

# clean NAs from the data
d2 <- d
d2$recipient <- NULL

# re-use map fit to get the formula
m10.3stan <- map2stan( m10.3 , data=d2 , iter=1e4 , warmup=1000 )
precis(m10.3stan)

## 10.13
pairs(m10.3stan)

## 10.14
m10.4 <- map2stan(
  alist(
    pulled_left ~ dbinom(1, p),
    logit(p) <- a[actor] + (bp + bpC*condition)*prosoc_left,
    a[actor] ~ dnorm(0, 10),
    bp ~ dnorm(0, 10),
    bpC ~ dnorm(0, 10)
  ),
  data = d2, chains = 2, iter = 2500, warmup = 500
)

## 10.16
precis(m10.4, depth = 2)

## 10.17
post <- extract.samples( m10.4 )
str(post)

## 10.18
dens( post$a[,2] )

## 10.19
chimp <- 1
d.pred <- list(
  pulled_left = rep( 0 , 4 ), # empty outcome
  prosoc_left = c(0,1,0,1),   # right/left/right/left
  condition = c(0,0,1,1),     # control/control/partner/partner
  actor = rep(chimp,4)
)
link.m10.4 <- link( m10.4 , data=d.pred )
pred.p <- apply( link.m10.4 , 2 , mean )
pred.p.PI <- apply( link.m10.4 , 2 , PI )
plot( 0 , 0 , type="n" , xlab="prosoc_left/condition" ,
      ylab="proportion pulled left" , ylim=c(0,1) , xaxt="n" ,
      xlim=c(1,4) , yaxp=c(0,1,2) )
axis( 1 , at=1:4 , labels=c("0/0","1/0","0/1","1/1") )
mtext( paste( "actor" , chimp ) )
p <- by( d$pulled_left ,
         list(d$prosoc_left,d$condition,d$actor) , mean )
lines( 1:4 , as.vector(p[,,chimp]) , col=rangi2 , lwd=2 )
lines( 1:4 , pred.p )
shade( pred.p.PI , 1:4 )

## 10.20
data(chimpanzees)
d <- chimpanzees
d.aggregated <- aggregate( d$pulled_left ,
                           list(prosoc_left=d$prosoc_left,condition=d$condition,actor=d$actor) ,
                           sum )

## 10.21
m10.5 <- map(
  alist(
    x ~ dbinom( 18 , p ) ,
    logit(p) <- a + (bp + bpC*condition)*prosoc_left ,
    a ~ dnorm(0,10) ,
    bp ~ dnorm(0,10) ,
    bpC ~ dnorm(0,10)
  ), data=d.aggregated )
precis(m10.5)

## 10.22
library(rethinking)
data(UCBadmit)
d <- UCBadmit

## 10.23
d$male <- ifelse( d$applicant.gender=="male" , 1 , 0 )
m10.6 <- map(
  alist(
    admit ~ dbinom( applications , p ) ,
    logit(p) <- a + bm*male ,
    a ~ dnorm(0,10) ,
    bm ~ dnorm(0,10)
  ),
  data=d )
m10.7 <- map(
  alist(
    admit ~ dbinom( applications , p ) ,
    logit(p) <- a ,
    a ~ dnorm(0,10)
  ), data=d )

## 10.24
compare(m10.6, m10.7)

## 10.25
precis(m10.6)

## 10.26
samples <- extract.samples(m10.6)
p.admit.male <- logistic(samples$a + samples$bm)
p.admit.female <- logistic(samples$a)
diff.p <- p.admit.male - p.admit.female
quantile(diff.p, c(.025, .5, .975))

## 10.27
postcheck( m10.6 , n=1e4 )
for ( i in 1:6 ) {
  x <- 1 + 2*(i-1)
  y1 <- d$admit[x]/d$applications[x]
  y2 <- d$admit[x+1]/d$applications[x+1]
  lines( c(x,x+1) , c(y1,y2) , col=rangi2 , lwd=2 )
  text( x+0.5 , (y1+y2)/2 + 0.05 , d$dept[x] , cex=0.8 , col=rangi2 )
}