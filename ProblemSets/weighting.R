
library(pacman)
p_load(dplyr, data.table, magrittr, estimatr)

set.seed(123)
n = 1e2
tmp = tibble(
  group = rep(0:1, each = n/2),
  x = rnorm(n = n, mean = 0 + group, sd = 1 + group * 2),
  y = 3 + (1 + group) * x + rnorm(n),
) %>% data.table()

g01 <- lm_robust(y ~ x + group, data = tmp)
# g01 <- lm_robust(y ~ x, data = tmp)
g0 <- lm_robust(y ~ x, data = tmp[group == 0])
g1 <- lm_robust(y ~ x, data = tmp[group == 1])

x_mean <- mean(tmp$x)
v01 <- tmp[, (x - mean(x))^2] %>% sum() %>% divide_by(n)
v0 <- tmp[group == 0, (x - x_mean)^2] %>% sum() %>% divide_by(n)
v1 <- tmp[group == 1, (x - x_mean)^2] %>% sum() %>% divide_by(n)

s0 <- v0 / (v0 + v1)
s1 <- v1 / (v0 + v1)

var0 <- tmp[group == 0, (x - mean(x))^2] %>% sum() %>% divide_by(n)
var1 <- tmp[group == 1, (x - mean(x))^2] %>% sum() %>% divide_by(n)

shr0 <- var0 / (var0 + var1)
shr1 <- var1 / (var0 + var1)

g01
shr0 * tidy(g0)[2,2] + shr1 * tidy(g1)[2,2]
s0 * tidy(g0)[2,2] + s1 * tidy(g1)[2,2]

# What happens if you do not include 'group' in your g01 regression?
