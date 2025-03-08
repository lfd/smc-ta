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

# A. Read Data
{
  options <- config$input
  context <- names(options$context)

  event.log <- fread(
    options$filename,
    col.names = c("timestamp", "event", context),
    colClasses = list("numeric" = 1, "factor" = 2, "factor" = (3:(3 + length(options$context) - 1)))
  )
  setorderv(event.log, cols = c("timestamp", context))

  if (!is.null(options$time_scale)) {
    event.log[, timestamp := options$time_scale(timestamp)]
  }

  if (config$print) {
    logger::log_info("Read {scales::number(nrow(event.log))} events from CSV file")
    event.log[, .(count = .N), by = "event"] |> print()
  }

  stopifnot(all(c("timestamp", "event") %in% colnames(event.log)))
}

if (config$name == "cyclictest") {
  # Remove context "cpu" due to implementation limitations / issues in filter_events
  event.log <- event.log[cpu == "1"]
  event.log[, cpu := NULL]
  config$data$context <- NULL
}

{
  source(here::here("lib2/filter_events.R"))
  source(here::here("lib2/transform1.R"))
  source(here::here("lib2/transform2.R"))
  source(here::here("lib2/select_extrems.R"))

  transitions <- event.log |>
    filter_events(config$data) |>
    transform1(config$data$context)
  source.data <- select_extrems(transitions, config$`eva-selection`)
  path.data <- transform2(transitions, config$data$context)
  rm(transitions)
}

if (F) {
  options <- config$data

  # B. Filtering
  {
    source(here::here("lib2/filter_events.R"))

    event.log <- filter_events(event.log, options = config$data)

    # Save for caching
    {
      file <- here::here(".cache", config$name, "event_log.rds")
      dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
      saveRDS(event.log, file)
      log_debug("Saved event.log")
    }

    # info_log(event.log)
  }


  # Assertion: given start_events must be an super set of our detected start events
  stopifnot(all(event.log[, .(start = first(event)), by = run]$start %in% options$start_events))
  if (!is.null(options$end_events)) {
    stopifnot(all(event.log[, .(end = last(event)), by = run]$end %in% options$end_events))
  }

  ##
  # Transform into TRANSITION format
  ##
  {
    log_info("Transform event log to transition log")

    source(here::here("lib2/transform1.R"))

    trans.log <- transform1(event.log, options$context)

    # Assertions
    {
      stopifnot(c("run", "event.from", "event.to", "duration") %in% colnames(trans.log))
    }

    # info_transition(trans.log)
  }


  ##
  # Step Probabilities
  ##
  {
    log_info("Derive transition probabilities")

    source(here::here("lib2/derive_P.R"))
    options <- config$data

    # 1-step
    trans.probs <- derive_P(trans.log)

    # n-step
    P <- format_probs_matrix(trans.probs, options$start_events)
    P1 <- P[[1]]
  }


  ##
  # Derive Paths
  ##
  {
    log_info("Tranform transitions to paths")

    source(here::here("lib2/transform2.R"))
    options <- config$data

    path.log <- transform2(trans.log, by = options$context)

    # info_path(path.log) |> print()
  }
}

{
  options <- config$model

  source(here::here("lib2/create_model.R"))
  model <- source.data |>
    create_model(options)

  saveRDS(model, file = here::here("data", config$name, "model.rds"))

  generate_mixture_density <- function(fit) {
    required_cols <- c("w", "mu", "sigma")
    stopifnot(all(required_cols %in% colnames(fit)))
    stopifnot(all(fit[, "sigma"] > 0))

    return(config$misc$d_generator(fit))

    n_row <- nrow(fit)
    if (n_row == 2) {
      return(funs[["mixture2"]])
    } else if (n_row == 3) {
      return(funs[["mixture3"]])
    } else if (n_row == 4) {
      return(funs[["mixture4"]])
    }

    abort()
  }

  t <- as.data.frame(model$Q)
  t$from <- rownames(t)
  p <- tidyr::pivot_longer(t, cols = -from, names_to = "to", values_to = "fit") |>
    dplyr::filter(!purrr::map_lgl(fit, is.null)) |>
    dplyr::mutate(
      mixture_density = purrr::map(fit, generate_mixture_density),
      transition = paste(from, to, sep = "→")
    )


  require(ggplot2)
  t_plot <- source.data |>
    filter(!is.na(event.to)) |>
    mutate(transition = paste(event.from, event.to, sep = "→")) |>
    ggplot() +
    scale_x_continuous("Holding Time", limits = c(0, NA), labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    scale_y_continuous("Density [a.u.]") +
    #stat_bin(aes(x = duration, y = after_stat(density)), binwidth = config$input$time_scale(0.5)) +
    stat_bin(aes(x = duration, y = after_stat(density)), bins = 100) +
    facet_wrap(~transition, scales = "free") +
    scale_color_discrete(NULL, label = \(x) stringr::str_to_title(x)) +
    theme_bw() +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), legend.position = "bottom")
  for (i in 1:nrow(p)) {
    t_plot <- t_plot +
      geom_function(data = p[i, ], aes(color = "model"), fun = p[[i, "mixture_density"]][[1]])
  }
  if (stringr::str_starts(config$name, "generated")) {
    s_f <- 1 / config$input$time_scale(1)
    t_plot <- t_plot +
      geom_function(data = tibble(transition = "a→b"), aes(color = "base"), fun = \(x) s_f * dnorm(s_f * x, mean = 12, sd = 4)) +
      geom_function(data = tibble(transition = "a→c"), aes(color = "base"), fun = \(x) s_f * dnorm(s_f * x, mean = 3, sd = 1)) +
      geom_function(data = tibble(transition = "c→d"), aes(color = "base"), fun = \(x) s_f * dweibull(s_f * x, shape = 1, scale = 1.2)) +
      geom_function(data = tibble(transition = "b→e"), aes(color = "base"), fun = \(x) s_f * evd::dgev(s_f * x, loc = 8.5, scale = 0.6, shape = -0.7)) +
      geom_function(data = tibble(transition = "e→d"), aes(color = "base"), fun = \(x) s_f * dgamma(s_f * x, shape = 2, scale = 2))
    if (stringr::str_starts(config$name, "generated-recurring")) {
      t_plot <- t_plot +
        geom_function(data = tibble(transition = "e→b"), aes(color = "base"), fun = \(x) s_f * dunif(s_f * x, min = 2, max = 7))
    }
  }
  t_plot |> print()
  if (F) {
    ggsave(here::here("img", config$name, "transitions.png"), width = 20, height = 15, units = "cm")
  }
}

# Compare models with AIC
if (F) {
  calculate_aic <- function(fit, k) {
    if (is.null(fit)) {
      return(NA)
    }
    # per Component(kx): 1x Mean + 1x Variance
    # + k-1 proportions (since they sum to 1)
    num_params <- 3 * k - 1

    # AIC calculation
    aic <- 2 * num_params - 2 * attr(fit, "loglik")
    return(aic)
  }

  t <- as.data.frame(model$Q)
  t$from <- rownames(t)
  p <- tidyr::pivot_longer(t, cols = -from, names_to = "to", values_to = "fit") |>
    dplyr::filter(!purrr::map_lgl(fit, is.null)) |>
    rowwise() |>
    mutate(aic = calculate_aic(unlist(fit, recursive = FALSE), k = 2))
}

# Evaluation
{
  options <- config$simulation

  source(here::here("lib2/evaluate_method.R"))

  path.fit <- source.data |>
    evaluate_method(
      N = 10000,
      options,
      Q = model$Q,
      P = model$P,
      return.type = "path"
    )
  saveRDS(path.fit, file = here::here("data", config$name, "path-fit.rds"))

  require(ggplot2)
  data <- rbindlist(
    list(
      mutate(path.fit, source = "model"),
      mutate(path.data, source = "measurement") |> select(!length)
    ),
    use.names = TRUE
  )
  m_plot <- data |>
    ggplot(aes(x = duration, color = source)) +
    # stat_bin(aes(y = after_stat(ncount)), binwidth = 1e-5, position = "dodge") +

    stat_bin(
      data = filter(data, source == "model"),
      aes(y = after_stat(density)),
      bins = 100,
      #binwidth = 1e-5,
      na.rm = TRUE,
      geom = "line") +
    stat_bin(
      data = filter(data, source == "measurement"),
      aes(y = after_stat(density)),
      bins = 100,
      #binwidth = 1e-5,
      na.rm = TRUE,
      geom = "line") +

    # stat_density(aes(y = after_stat(density)), geom = "line", position = "identity") +

    scale_x_continuous("Absorbing Time", limits = c(0, NA), labels = scales::label_number(scale_cut = scales::cut_si("s")), expand = expansion(add = c(0, 0), mult = c(0, .05))) +
    scale_y_continuous("Density [a.u.]", breaks = c(0)) +
    scale_color_discrete(NULL, label = \(x) stringr::str_to_title(x)) +
    theme_bw() +
    theme(
      # axis.text.y = element_blank(),
      # axis.ticks.y = element_blank(),
      legend.position = "bottom"
    )

  print(m_plot)
  if (F) {
    ggsave(here::here("img", config$name, "latency.png"), width = 20, height = 15, units = "cm")
  }
  m_plot +
    coord_cartesian(xlim = c(quantile(data[source == "measurement", duration], .95), NA), ylim = c(0, 0.05 * 1e5)) +
    geom_point(data = data[, .(max = max(duration)), by = c("source")], aes(x = max, y = 0)) +
    # geom_label(data = data[, .(max= max(duration)), by = c("source")], aes(x = max, y = 0, label = scales::label_number(scale_cut = scales::cut_si("s"))(max)), nudge_y = 0.0015e5, show.legend = FALSE) +
    labs(x = "Absorbing Time (> 95% quantile)")
  if (F) {
    ggsave(here::here("img", config$name, "latency-tail.png"), width = 20, height = 15, units = "cm")
  }

  q_plot <- data |>
    as.data.frame() |>
    reframe(
      q = c(.75, .8, .85, .9, .95, .99, .999, 1),
      q_name = ordered(q, labels = scales::label_percent(0.1)(q)),
      value = quantile(duration, q),
      .by = c("source")
    ) |>
    ggplot(aes(x = q_name, y = value, color = source)) +
    geom_point() +
    scale_y_continuous("Holding Time", limits = c(0, NA), labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    labs(x = "Quantile") +
    theme_bw() +
    scale_color_discrete(NULL, label = \(x) stringr::str_to_title(x)) +
    theme(legend.position = "bottom")

  print(q_plot)

  if (F) {
    ggsave(here::here("img", config$name, "quantiles.png"), width = 20, height = 15, units = "cm")
  }
}
