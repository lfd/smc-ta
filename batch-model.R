library(rlang)
library(data.table)
library(logger)

# Set config via "R_CONFIG_ACTIVE"

{
  TIME_SCALES <- list(
    ms = \(x) x * 1e-3,
    us = \(x) x * 1e-6,
    ns = \(x) x * 1e-9,
    ps = \(x) x * 1e-12
  )
  FIT_FUNS <- list(
    mixture2 = \(vec) fit_mixture(vec, 2),
    mixture3 = \(vec) fit_mixture(vec, 3),
    mixture4 = \(vec) fit_mixture(vec, 4)
  )
  METHODS <- list(
    mixture2 = list(
      fit_fun = \(vec) fit_mixture(vec, 2),
      sample_fun = \(n = 1, params) nor1mix::rnorMix(n, params),
      d_generator = \(fit) function(x) {
        fit[1, "w"] * dnorm(x, mean = fit[1, "mu"], sd = fit[1, "sigma"]) +
          fit[2, "w"] * dnorm(x, mean = fit[2, "mu"], sd = fit[2, "sigma"])
      }
    ),
    mixture3 = list(
      fit_fun = \(vec) fit_mixture(vec, 3),
      sample_fun = \(n = 1, params) nor1mix::rnorMix(n, params),
      d_generator = \(fit) function(x) {
        fit[1, "w"] * dnorm(x, mean = fit[1, "mu"], sd = fit[1, "sigma"]) +
          fit[2, "w"] * dnorm(x, mean = fit[2, "mu"], sd = fit[2, "sigma"]) +
          fit[3, "w"] * dnorm(x, mean = fit[3, "mu"], sd = fit[3, "sigma"])
      }
    ),
    mixture4 = list(
      fit_fun = \(vec) fit_mixture(vec, 4),
      sample_fun = \(n = 1, params) nor1mix::rnorMix(n, params),
      d_generator = \(fit) function(x) {
        fit[1, "w"] * dnorm(x, mean = fit[1, "mu"], sd = fit[1, "sigma"]) +
          fit[2, "w"] * dnorm(x, mean = fit[2, "mu"], sd = fit[2, "sigma"]) +
          fit[3, "w"] * dnorm(x, mean = fit[3, "mu"], sd = fit[3, "sigma"]) +
          fit[4, "w"] * dnorm(x, mean = fit[4, "mu"], sd = fit[4, "sigma"])
      }
    )
  )
}

{
  config <- config::get(use_parent = FALSE)
  log_debug("Config read")

  stopifnot(length(config$data$start_events) > 0)
  stopifnot(length(config$data$end_events) > 0)

  config$name <- config$name %||% Sys.getenv("R_CONFIG_ACTIVE", unset = NA) %|% "default"
  config$print <- config$print %||% TRUE

  config$input$context <- config$data$context
  config$data$context <- names(config$data$context)

  config$model$initial_states <- config$data$start_events
  config$model$absorbing_states <- config$data$end_events

  config$simulation$start_events <- config$data$start_events
  config$simulation$end_events <- config$data$end_events
  config$simulation$sample_fun <- METHODS[[config$model$method]]$sample_fun

  config$misc$d_generator <- METHODS[[config$model$method]]$d_generator

  config$model$fit_fun <- METHODS[[config$model$method]]$fit_fun
  config$input$time_scale <- TIME_SCALES[[config$input$time_scaling]]

  if (is.null(config$input$time_scaling)) {
    warn("Time scaling unspecified, assume no scaling")
  } else if (!config$input$time_scaling %in% names(TIME_SCALES)) {
    warn("Unkown time scale {config$input$time_scaling}")
  }
  if (!config$model$method %in% names(FIT_FUNS)) {
    warn("Unkown time scale {config$model$method}")
  }

  rm(FIT_FUNS, TIME_SCALES, METHODS)
}


{
  transitions <- readRDS(here::here("data", config$name, "transitions.rds"))

  N <- max(transitions$run)
  ns <- seq(from = 1e3, to = N, by = 1e3)


  source(here::here("lib2/create_model.R"))
  source(here::here("lib2/evaluate_method.R"))
  source(here::here("lib2/select_extrems.R"))
  # Evaluation Option 3:
  # 1. Measure
  # 2. Fit Model
  # 3. Simulate Runs

  start.time <- Sys.time()

  library(future)
  library(dplyr)
  NPROC <- 15
  future::plan(multicore, workers = NPROC)

  model_max <- ns[-(1:10)] |>
    furrr::future_map(
    #purrr::map(
      function(n) {
        t <- transitions |>
          filter(run <= (N %/% n) * n) |>
          group_split(ceiling(run / n), .keep = F)

        #.pmap <- NULL
        #if (length(t) < 2) {
          .pmap <- purrr::pmap
        #} else {
        #  .pmap <- furrr::future_pmap
        #}

        .pmap(
          list(t, seq_along(t)),
          function(t, idx) {
            N <- 10000
            t <- as.data.table(t) |>
              select_block_maxima(10)
            model <- create_model(t, options = config$model)
            max <- t |>
              evaluate_method(
                N = N,
                P = model$P,
                Q = model$Q,
                options = config$simulation,
                return.type = "path"
              ) |>
              slice_max(duration)


            list(
              N = N,
              n = n,
              max = max$duration,
              group = idx
            )
          }
        ) |>
          rbindlist()
      }
    ) |>
    rbindlist()
  end.time <- Sys.time()

  saveRDS(model_max, here::here("data", config$name, "_model_max-10000-bmm10_rem.rds"))

  diff <- end.time - start.time

  print(glue::glue("Model Evaluation Time: Start={start.time} - End={end.time} = {diff}"))
}
