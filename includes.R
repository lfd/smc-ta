set.seed(772711) # for reproducibility
options(warnPartialMatchDollar = TRUE)
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")
options(warnPartialMatchArgs = TRUE)

library(here)

#library(dplyr)

library(data.table)
library(dtplyr)
library(multidplyr)
library(dplyr, warn.conflicts = FALSE)

library(ggplot2)
library(stringr)
#library(tidyverse)

library(reshape2) # data.frame -> matrix (used for probailities)

library(ggstats)
library(ggridges)
library(igraph)

library(furrr)
library(progressr)
plan(sequential)
#plan(multisession, workers = 8)
#plan(cluster, workers = rep("im-srv-005.hs-regensburg.de", 10))
# plan(multicore, workers = 10) # unstable in RStudio

# library(flexsurv)
#library(MASS) # fitdistr to fit distributions
library(fitdistrplus, include.only = c("fitdist"))
library(mixtools) # Mixture of Gaussian

library(tikzDevice)
options(tikzDefaultEngine = 'pdftex')

ggplot2::theme_update(plot.margin = unit(c(0,0,0,0), 'cm'))

list.files(path = here('lib'), pattern = "\\.R$", full.names = TRUE) |>
	sapply(source)

dir.create(OUT_DIR, showWarnings = FALSE)

save_func <- function(plot = last_plot(), filename, ...) {
  # ggplot2::ggsave(filename = str_c(filename, ".tex"), plot, path = OUT_DIR, device = tikz, width = 7, height = 4)

  HEIGHT <- 0.75 * COL.WIDTH

  message(paste(OUT_DIR, filename, sep = "/"))
	# TIKZ
  #tikz(str_c(OUT_DIR, "/", filename, ".tex"), width = COL.WIDTH, height = HEIGHT, sanitize = TRUE)
  #print(plot)
  #dev.off()

  # PNG for preview purposes
  ggplot2::ggsave(filename = str_c(filename, ".png"),
                  plot,
                  path = str_c(OUT_DIR, "/", "big"),
                  width = 3000,
                  height = 2000,
                  units = "px",
                  ...)
  ggplot2::ggsave(filename = str_c("preview-", filename, ".png"),
                  plot,
                  path = OUT_DIR,
                  width = COL.WIDTH,
                  height = HEIGHT,
                  units = "in",
                  ...)
}

if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  handlers("rstudio")
}


h_probs <- c(.9, .99, .999, .9999, .99999)

h_methods <- list(
  gev = list(fit_fun = fit_gev, sample_fun = sample_gev),
  nmixture2 = list(fit_fun = fit_nmixture2, sample_fun = sample_nmixture),
  nmixture3 = list(fit_fun = fit_nmixture3, sample_fun = sample_nmixture),
  nmixture4 = list(fit_fun = fit_nmixture4, sample_fun = sample_nmixture),
  mixture2 = list(fit_fun = fit_mixture2, sample_fun = sample_mixture),
  mixture3 = list(fit_fun = fit_mixture3, sample_fun = sample_mixture),
  mixture4 = list(fit_fun = fit_mixture4, sample_fun = sample_mixture)
)
