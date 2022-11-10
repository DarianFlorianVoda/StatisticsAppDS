### Exercise 29

library(mosaic)
library(gridExtra)
library(ggplot2)
mu <- 1.2
sigma <- 0.02


# 1)
low.prob <- xpnorm(1.175, return="value", plot=FALSE, mean=mu, sd=sigma)
low.prob

# Plot

h2 = xpnorm(1.175, mean=mu, sd=sigma, return = "plot", system = "gg") %>% 
  gf_labs(title = "Lower bound probability", x = "Height") %>% 
  gf_theme(plot.title = element_text(hjust = 0.5, color = "navy"))


high.prob <- xpnorm(1.195, return="value", plot=FALSE, mean=mu, sd=sigma)
high.prob

h1 = xpnorm(1.195, mean=mu, sd=sigma, return = "plot", system = "gg") %>% 
  gf_labs(title = "Upper bound probability", x = "Height") %>% 
  gf_theme(plot.title = element_text(hjust = 0.5, color = "navy"))


grid.arrange(h2, h1, ncol=2)

diff.prob <- high.prob - low.prob
diff.prob

diff_plot = xcnorm(diff.prob, mean=mu, return="plot", sd=sigma, system="gg") %>%
  gf_labs(title = "Between probability", x = "Height") %>% 
  gf_theme(plot.title = element_text(hjust = 0.5, color = "black"))
diff_plot

# 2)
longer = 1 - xpnorm(1.190, return="value", mean=mu, sd=sigma)
longer

xqnorm(longer, mean=mu, return="plot", sd=sigma, system="gg") %>%
  gf_labs(title = "Probability longer than 1.19m", x = "Height") %>% 
  gf_theme(plot.title = element_text(hjust = 0.5, color = "black"))


# 3)

#value = mu + (sigma/sqrt(0.01))
#value

new_low.prob <- xpnorm(1.185, mean=mu, sd=sigma)
new_low.prob
new_high.prob <- xpnorm(1.205, mean=mu, sd=sigma)
new_high.prob
new_diff.prob <- new_high.prob - new_low.prob
new_diff.prob




new_plot = xcnorm(new_diff.prob, mean=mu, return="plot", sd=sigma, system="gg") %>%
  gf_labs(title = "Deviation of 0.01m", x = "Height") %>% 
  gf_theme(plot.title = element_text(hjust = 0.5, color = "black"))

grid.arrange(new_plot, diff_plot, ncol=2)

### Exercise 30

mu_2 = 5
sigma_2 = 0.3

# 1)
ex30_1 = xpnorm(4.4, mean=mu_2, sd=sigma_2)
ex30_1

# 2)
ex30_2 = 1-xpnorm(5.4, mean=mu_2, sd=sigma_2)
ex30_2


### Exercise 32

mu_3 = 100
sd_3 = 15

# 1)
mu_3 <- 100
sigma_3 <- 15
lower.bound.pct <- (1 - 0.95) / 2
upper.bound.pct <- (1 - 0.95) / 2 + 0.95
interval.95 <- c(qnorm(lower.bound.pct, mu_3, sigma_3),
                   qnorm(upper.bound.pct, mu_3, sigma_3))
interval.95


# 2)
ex32_2 = xpnorm(120, mean = mu_3, sd=sigma_3)
ex32_2

# 3)

ex32_3 = qnorm(0.95, mean=mu_3, sd=sigma_3)
ex32_3
