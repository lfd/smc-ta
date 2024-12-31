sample_mixture <- function(n = 1, params) {
  sample(1:length(params[["mixing_proportions"]]), size = n, replace = TRUE, prob = params[["mixing_proportions"]]) |>
    sapply(function(component) rnorm(1, mean = params[["mu"]][component], sd = params[["sd"]][component]))
}
