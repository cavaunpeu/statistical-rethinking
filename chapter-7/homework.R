# homework

## 7M1

# The fact that tulips cease to grow entirely when the temperature is hot, in lieu of shrinking in bloom-size in a linear fashion, implies a non-linear model.
# This said, we can point to an interaction between water, shade (which together were shown to be conducive to tulip growth) and temperature.

## 7M2

# A regression equation that would make the bloom size zero whenever the temperature is hot would include only terms interacted with an is_not_hot variable, such that when
# this variable is 0, i.e. it is *not* is_not_hot, the bloom size would be zero.

## 7M3

# The relationship between the amount of food that a raven consumes and the presence of a wolf killing this prey cannot be entirely linear, as without the wolf the
# raven would not eat any food at all. Once the wolf does enter the picture, though, the relationship could thereafter be a linear one.

## 7H1
library(rethinking)
data(tulips)
d <- tulips
str(d)

# center variables
d$water.centered <- d$water - mean(d$water)
d$shade.centered <- d$shade - mean(d$shade)
d$bed.b <- ifelse(d$bed == "b", 1, 0)
d$bed.c <- ifelse(d$bed == "c", 1, 0)

# fit model with interaction term plus main effects for unique levels of `bed`
m7H1 <- map(
  alist(
    blooms ~ dnorm( mu , sigma ) ,
    mu <- alpha + beta.water*water.centered + beta.shade*shade.centered + beta.water.shade*water*shade + beta.bed.b*bed.b + beta.bed.c*bed.c,
    c(alpha, beta.water, beta.shade, beta.water.shade, beta.bed.b, beta.bed.c) ~ dnorm( 0, 100 ),
    sigma ~ dunif( 0 , 100 )
  ),
  data=d,
  start=list(alpha=mean(d$blooms), beta.water=0, beta.shade=0, beta.water.shade=0, beta.bed.b=0, beta.bed.c=0, sigma=sd(d$blooms)), 
  control=list(maxit=1e4)
)

