# chapter 2

## 2.1
ways <- c(0, 3, 8, 9, 0)
ways / sum(ways)

## 2.2
dbinom(6, size = 9, prob = .5)

## 2.3 - posterior by hand
p_grid <- seq(from = 0, to = 1, length.out = 20)

prior.1 <- rep(x = 1, 20)
prior.2 <- ifelse(test = p_grid < .5, yes = 0, no = 1)
prior.3 <- exp( -5*abs( p_grid - .5 ) )
prior <- prior.3

likelihood <- dbinom(x = 6, size = 9, prob = p_grid)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)

# plot
plot(x = p_grid, y = posterior, type="b", xlab = "Probability of Water", ylab = "Posterior Probability")
mtext("20 Points")

## 2.6 - MAP
library(rethinking)

globe.qa <- map(
  alist(
    w ~ dbinom(size = 9, prob = p),
    p ~ dunif(min = 0, max = 1)
  ),
  data=list(w=6)
)

precis(globe.qa)

## 2.7 - analytical posterior vs. MAP

# analytical calculation
w <- 6
n <- 9
curve( dbeta(x = x, shape1 = w+1, shape2 = n-w+1), from = 0, to = 1)

# MAP calculation
curve( dnorm(x = x, mean = .67, sd = .16), from = 0, to = 1)
