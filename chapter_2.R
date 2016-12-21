# chapter 2

## 2.1
ways <- c(0, 3, 8, 9, 0)
ways / sum(ways)

## 2.2
dbinom(6, size = 9, prob = .5)

## 2.3

# compute posterior
p_grid <- seq(from = 0, to = 1, length.out = 20)
prior <- rep(x = 1, 20)
likelihood <- dbinom(x = 6, size = 9, prob = p_grid)
unstardized.posterior <- likelihood * prior
posterior = unstardized.posterior / sum(unstardized.posterior)

# plot
plot(x = p_grid, y = posterior, type="b", xlab = "Probability of Water", ylab = "Posterior Probability")
mtext("20 Points")
