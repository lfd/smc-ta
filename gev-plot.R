
source("includes.R")

library(ggplot2)
library(dplyr)
library(evd)

OUT_DIR <- here::here()

ggplot() +
	geom_function(aes(color = "reversed Weibull"), xlim = c(-10, 2), fun = evd::dgev, args = list(loc = 0, scale = 1, shape = -0.5)) +
	geom_function(aes(color = "Gumbel"), xlim = c(-3, 10), fun = evd::dgev, args = list(loc = 0, scale = 1, shape = 0)) +
	geom_function(aes(color = "Fréchet"), fun = evd::dgev, args = list(loc = 0, scale = 1, shape = 0.5)) +
	scale_x_continuous(limits = c(-4,6))+
	scale_y_continuous("Density") +
	scale_color_discrete("Distribution") +
  theme_minimal() +
	labs(
		#title = "Probability Density Functions",
       x = NULL,
       y = "Density",
       color = "Distribution"
       ) +
	theme_minimal() +
  theme(legend.position = "right")

save_for_thesis(filename = "gev", prop= 0.4)
