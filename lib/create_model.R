create_model <- function(transitions, options) {
  source(here::here("lib/transform1.R"))
  source(here::here("lib/derive_P.R"))
  source(here::here("lib/fit.R"))
  source(here::here("lib/fit_mixture.R"))

  required_fields <- c("initial_states", "absorbing_states", "fit_fun")
  stopifnot(all(required_fields %in% names(options)))

  pi <- transitions[, .(event = first(event.from)), by = run][, .(.N), by = event][, .(event, prob = N / sum(N))]

  P <- transitions |>
    derive_P() |>
    format_probs_matrix(options$initial_states, max_iter = 1) |>
    first()

  # Q <- transitions |>
  #   as.data.frame() |>
  #   method_fit(fit_fun) |>
  #   dplyr::mutate(transition = paste(event.from, event.to, sep = "→"))

  Q <- transitions |>
    as.data.frame() |>
    method_fit.matrix(options$fit_fun)

  list(
    initial_states = options$initial_states,
    absorbing_states = options$absorbing_states,
    pi = pi,
    P = P,
    Q = Q
  )
}
