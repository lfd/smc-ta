conv_markov_to_fitted_transition <- function(df) {
  .df <- df |>
    # dplyr::mutate(censrec = if_else(is.na(event.to), 0, 1)) |>
    # dplyr::select(run, event.from, event.to, duration, censrec) |>

    # Filter legacy data
    # filter(!(state.j == state.h & time == 0)) %>%

    dplyr::select(!transition) |>
    dplyr::filter(!is.na(duration))

  result <- list()

  # .conv_markov_to_fitted_transition_surv(df)
  result$weibull <- .conv_markov_to_fitted_transition_fitdist(.df)
  # result$mixture <- .conv_markov_to_mixture_gaussian(.df)
  result$mixture2 <- .conv_markov_to_mixture_gaussian(.df, k = 2)
  result$mixture3 <- .conv_markov_to_mixture_gaussian(.df, k = 3)
  result$mixture4 <- .conv_markov_to_mixture_gaussian(.df, k = 4)

  return(result)
}

.conv_markov_to_mixture_gaussian <- function(df, k = NA, diagnosis = F) {
  fit.mixtureGaussian <- function(vec) {
    if (is.na(k) || is.null(k)) {
      warning("number of gaussians is not provided")
      k <- 2 * floor(log10(length(vec)))
    }
    # Estimate the parameters of a mixture of Gaussian distributions
    estimation <- mixtools::normalmixEM(vec, k = k)

    # Returning a list of parameters
    params <- list(
      mu = estimation$mu,
      sd = sqrt(estimation$sigma^2), # sigma^2 are the variances
      mixing_proportions = estimation$lambda
    )

    return(params)
  }

  data.model <- df |>
    dplyr::summarise(
      .scale = attributes(scale(duration, center = FALSE))[["scaled:scale"]],
      params = fit.mixtureGaussian(duration / .scale) |> list(),
      .by = c(event.from, event.to)
    ) |>
    tidyr::unnest_wider(params) |>
    dplyr::mutate(mu = map2(mu, .scale, ~ .x * .y), sd = map2(sd, .scale, ~ .x * .y), .keep = "unused") |>
    dplyr::mutate(
      params = c(mixing_proportions = mixing_proportions, mu = mu, sd = sd) |> list(),
      .by = c(event.from, event.to),
      .keep = "unused"
    )

  data.model <- df |>
    dplyr::nest_by(event.from, event.to, .key = "raw_data") |>
    dplyr::mutate(
      .scale = attributes(scale(pluck(raw_data, "duration"), center = FALSE))$scale,
      params = fit.mixtureGaussian(pluck(raw_data, "duration") / .scale) |> list(),
      # If needed, update the simulated data generation step based on the Gaussian mixture model parameters
      # simulated_data = tibble(...) |> list(),
    ) |>
    dplyr::mutate(params = list(list(
      mixing_proportions = params[["mixing_proportions"]],
      mu = params[["mu"]] * .scale,
      sd = params[["sd"]] * .scale
    ))) |>
    dplyr::select(!.scale)

  # a <- result |>
  #  dplyr::mutate(
  #    density = list(
  #      function(p, n = 1) {
  #        apply(
  #          sapply(
  #            1:length(p$mixing_proportions),
  #            function(i) { rnorm(n, mean = p$mu[i], sd = sqrt(p$sd[i])) * p$mixing_proportions[i] }
  #          ),
  #          1,
  #          sum
  #        )
  #      }
  #    )
  #  )
  return(data.model)
}

.conv_markov_to_fitted_transition_fitdist <- function(df, diagnosis = FALSE) {
  fit.weibull <- function(vec) {
    estimation <- fitdistrplus::fitdist(vec, "weibull")

    # diagnostic
    if (diagnosis) {
      plot(estimation)
      print(summary(estimation))
    }

    y <- summary(estimation)[[1]]

    return(y)
  }

  # 1. scale data
  # 2. estimate distribution
  # 3. (re)scale estimation params
  data.model <- df |>
    dplyr::summarise(
      .scale = attributes(scale(duration, center = FALSE))[["scaled:scale"]],
      params = fit.weibull(duration / .scale) |> list(),
      .by = c(event.from, event.to)
    ) |>
    tidyr::unnest_wider(params) |>
    dplyr::mutate(scale = scale * .scale, .keep = "unused") |>
    # dplyr::nest_by(event.from, event.to, .key = "params")
    dplyr::mutate(params = c(shape = shape, scale = scale) |> list(), .by = c(event.from, event.to), .keep = "unused")

  return(data.model)
}

.debug_single_fit <- function(df, row_nr = 1) {
  raw <- unnest(df[row_nr, ], raw_data)$duration
  s <- attributes(scale(raw, center = FALSE))$`scaled:scale`
  p <- fitdistrplus::fitdist(raw / s, "weibull", method = "mle") # , control = list(trace = 1, REPORT = 1))
  shape <- p$estimate[["shape"]]
  scale <- p$estimate[["scale"]] * s

  p <- data.frame(x = raw) |>
    ggplot2::ggplot(aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), color = "black", fill = "white") +
    stat_bin(aes(y = after_stat(density)), geom = "point") +
    geom_function(fun = function(x) dweibull(x, shape = shape, scale = scale), color = "red")

  print(p + scale_y_log10(limits = c(1e-15, NA)))
  print(p)

  return()
}
# .debug_single_fit(data.model, 2)



# .conv_markov_to_fitted_transition_surv <- function(data) {
#   if (!require("flexsurv", character.only = TRUE)) {
#     stop("Package 'flexsurv' is required but not installed")
#   }
#   if (missing(data)) {
#     stop("Argument 'data' is missing with no default")
#   }
#
#   required_columns <- c("duration", "censrec", "event.from", "event.to")
#   stopifnot(all(required_columns %in% colnames(data)))
#
#   results <- data %>%
#     dplyr::nest_by(event.from, event.to, .key = "subdata") %>%
#     dplyr::mutate(
#       model = flexsurvreg(
#         Surv(duration, censrec) ~ 1,
#         data = subdata,
#         dist = "weibull",
#         # lower params: shape, scale, covariates
#         lower = c(1e-15, -Inf, -Inf),
#         method = "L-BFGS-B",
#         # control = c( # trace = 1,
#         #  fnscale = 1e-9
#         # )
#       ) |> list(),
#       params = coef(model) |> list(), # TODO scale by default if this functions is reactivated!!!!
#       fitted_data = list(tibble(duration = rweibull(
#         1000, 1 / params[["shape"]], exp(params[["scale"]])
#       ))),
#       # .keep = "unused"
#     )
#
#   return(results)
# }
