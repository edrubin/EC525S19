library(pacman)
p_load(dplyr, ggplot2, ggthemes, viridis, magrittr)


# Set seed
set.seed(123)
# Sample size
n <- 1e3
# Generate data
pop_df <- tibble(
  e = rnorm(n),
  c = rep(c(0,1), n/2),
  τ = rnorm(n, mean = 1, sd = 0.5),
  u = rnorm(n, mean = 2 * c),
  y0 = e + u,
  y1 = e + u + τ
)
gg_df <- tibble(
  i = rep(1:1e3, 2),
  c = rep(pop_df$c, 2),
  y = c(pop_df$y0, pop_df$y1),
  outcome = rep(c("y[0]", "y[1]"), each = n)
)

# Plot
ggplot(
  data = gg_df,
  aes(x = c, y = y)
) +
geom_jitter() +
theme_pander() +
facet_grid(. ~ outcome)
