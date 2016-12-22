# homework

## 3M1
w <- 8
n <- 15
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(x = 1, length(p_grid))
likelihood <- dbinom(x = w, size = n, prob = p_grid)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
plot(posterior ~ p_grid, type = "l")

## 3M2
trials <- 1e4
samples <- sample(x = p_grid, size = trials, prob = posterior, replace = TRUE)
HPDI(samples = samples, prob = .9)

## 3M3
posterior.predictive.distribution <- rbinom(n = trials, size = n, prob = samples)
mean(posterior.predictive.distribution == 8)

## 3M4
posterior.predictive.distribution <- rbinom(n = trials, size = 9, prob = samples)
mean(posterior.predictive.distribution == 6)

## 3M5

# 3M1
w <- 8
n <- 15
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- ifelse(test = p_grid < .5, yes = 0, no = 1)
likelihood <- dbinom(x = w, size = n, prob = p_grid)
unstandardized.posterior <- likelihood * prior
posterior <- unstandardized.posterior / sum(unstandardized.posterior)
plot(posterior ~ p_grid, type = "l")

# 3M2
trials <- 1e4
samples <- sample(x = p_grid, size = trials, prob = posterior, replace = TRUE)
HPDI(samples = samples, prob = .9)

# 3M3
posterior.predictive.distribution <- rbinom(n = trials, size = n, prob = samples)
mean(posterior.predictive.distribution == 8)

# 3M4
posterior.predictive.distribution <- rbinom(n = trials, size = 9, prob = samples)
mean(posterior.predictive.distribution == 6)