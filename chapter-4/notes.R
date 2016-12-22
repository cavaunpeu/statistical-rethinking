# notes

## 4.1 - simulate random walks
final.positions <- replicate(n = 1000, sum(runif(n = 16, min = -1, max = 1)))

## 4.3 - simulate multiplicative growth rates
growth <- replicate(n = 1e5, expr = prod(1 + runif(n = 12, min = 0, max = .01)))
dens(growth, norm.comp = TRUE)

## 4.4 - simulate multiplicative growth rates for big and small individual percetile growth ranges
growth.small <- replicate(n = 1e5, expr = prod(1 + runif(n = 12, min = 0, max = .01)))
growth.big <- replicate(n = 1e5, expr = prod(1 + runif(n = 12, min = 0, max = .5)))

## 4.5 - simulate log-multiplicative growth rates
growth <- replicate(n = 1e5, expr = log( prod(1 + runif(n = 12, min = 0, max = .01)) ))
dens(growth, norm.comp = TRUE)

## 4.7
library(rethinking)
data(Howell1)
d <- Howell1

## 4.10
d2 <- d[ d$age >= 18 , ]

