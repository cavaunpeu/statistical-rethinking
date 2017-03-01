# homework

## 11M1
ratings <- c(12, 36, 7, 41)
ratings.proportions <- ratings / sum(ratings)
ratings.proportions.cdf <- cumsum(ratings.proportions)
log.ratings.proportions.cdf <- log(ratings.proportions.cdf)

## 11M3
# I believe this one is pretty straightforward. The only thing we must do is swap the Poisson likelihood (which accepts a lambda) with the Binomial
# likelihood (which accepts both a probability of success `p` and the number of trials `N`). As such, our new likelihood of a non-zero value is 
# given by: (1 - p_not_work)Pr(y | N, p).

