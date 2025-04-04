library(rlang)
library(data.table)
library(logger)

# Set config via "R_CONFIG_ACTIVE"


source(here::here("_params.R"))

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
  paths <- transform2(transitions)

  N <- nrow(paths)
  ns <- seq(from = 1e3, to = N, by = 1e3)

  # Evaluation OPtion 1:
  # 1. Measure Data
  # 2. Take Maximum (Total) Duration
  data_max <- ns |>
    purrr::map(
      function(n) {
        paths |>
          slice_head(n = (N %/% n) * n) |>
          group_split(ceiling(row_number() / n), .keep = F) |>
          purrr::imap(function(x, idx) {
            list(
              n = n,
              data_max = max(x$duration),
              group = idx
            )
          }) |>
          rbindlist()
      }
    ) |>
    rbindlist()

  saveRDS(data_max, here::here("data", config$name, "_data_max.rds"))

  data_max |>
    # filter(n < N / 3) |>
    ggplot(aes(x = n, y = max)) +
    geom_boxplot(aes(group = (2 * n %/% 1e4))) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    theme_bw()


  # Evaluation Option 2:
  # 1. Measure
  # 2. Retrieve maximum duration per Transition
  # 3. Take longest Path
  # 4. Sum max transition durations of this path
  trans_max <- ns |>
    purrr::map(
      function(n) {
        t <- transitions |>
          filter(run <= (N %/% n) * n) |>
          group_split(ceiling(run / n), .keep = F)
        p <- paths |>
          slice_head(n = (N %/% n) * n) |>
          group_split(ceiling(row_number() / n), .keep = F)

        purrr::pmap(
          list(t, p, seq_along(p)),
          function(t, p, idx) {
            # Event-Sequence of path with longest (total/path) duration.
            max_seq <- p |>
              slice_max(duration) |>
              purrr::pluck("path") |>
              strsplit("→") |>
              first()

            # Take max. duration of each transition in path
            max <- data.table(
              event.from = max_seq[-length(max_seq)],
              event.to = max_seq[-1]
            ) |>
              left_join(
                t |>
                  filter(!is.na(event.to)) |>
                  summarise(max = max(duration), .by = c(event.from, event.to)),
                by = join_by(event.from, event.to)
              ) |>
              purrr::chuck("max") |>
              sum()

            list(
              n = n,
              max = max,
              group = idx
            )
          }
        ) |>
          rbindlist()
      }
    ) |>
    rbindlist()

  saveRDS(trans_max, here::here("data", config$name, "_trans_max.rds"))

  trans_max |>
    ggplot(aes(x = n, y = max)) +
    # geom_jitter() +
    geom_boxplot(aes(group = (2 * n %/% 1e4))) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    theme_bw()




  source(here::here("lib2/create_model.R"))

  source(here::here("lib2/transform1.R"))
  source(here::here("lib2/derive_P.R"))
  source(here::here("lib2/fit.R"))
  source(here::here("lib2/fit_mixture.R"))

  source(here::here("lib2/evaluate_method.R"))

  source(here::here("lib2", "simulate.R"))
  # Evaluation Option 3:
  # 1. Measure
  # 2. Fit Model
  # 3. Simulate Runs


  future::plan("future::sequential")
  library(future)
  library(dplyr)
  future::plan(multisession, workers = 11)

  parallel::stopCluster(cl)
  cl <- parallelly::makeClusterPSOCK(workers = rep("im-srv-011.hs-regensburg.de", 32), user = rep("benno", 32), rshopts = c("-i", "~/.ssh/id_ed25519_diginix"), homogeneous = FALSE, autoStop = T, verbose = T)
  cl <- parallelly::makeClusterPSOCK(15)
  future::plan(cluster, workers = cl)

  model_max <- ns |>
    furrr::future_map(
      # purrr::map(
      function(n) {
        t <- transitions |>
          filter(run <= (N %/% n) * n) |>
          group_split(ceiling(run / n), .keep = F)

        purrr::pmap(
          list(t, seq_along(t)),
          function(t, idx) {
            N <- 10000
            t <- as.data.table(t)
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

  saveRDS(model_max, here::here("data", config$name, "_model_max.rds"))


  data_max <- readRDS(here::here("data", config$name, "_data_max.rds"))
  trans_max <- readRDS(here::here("data", config$name, "_trans_max.rds"))
  model_max <- rbind(
    readRDS(here::here("data", config$name, "_model_max-100.rds")),
    readRDS(here::here("data", config$name, "_model_max-1000.rds")),
    readRDS(here::here("data", config$name, "_model_max-10000.rds")),
    readRDS(here::here("data", config$name, "_model_max-20000.rds"))
  ) |>
    cbind(eva = "") |>
    rbind(cbind(readRDS(here::here("data", config$name, "_model_max-1000-bmm2.rds")), eva = "BMM2")) |>
    rbind(cbind(readRDS(here::here("data", config$name, "_model_max-1000-bmm10.rds")), eva = "BMM10")) |>
    rbind(cbind(readRDS(here::here("data", config$name, "_model_max-10000-bmm2.rds")), eva = "BMM2"))

  readRDS(here::here("data", config$name, "_model_max-10000-bmm10_1_10.rds"))


  require(ggplot2)

  data <- rbindlist(list(
    cbind(data_max, eva = NA, N = NA, type = "data"),
    cbind(trans_max, eva = NA, N = NA, type = "transitions"),
    cbind(model_max, type = paste("model", model_max$N, model_max$eva))
  ), use.names = TRUE)

  data |>
    filter(type == "model 20000") |>
    # filter(group == 1) |>
    ggplot(aes(x = n, y = max)) +
    geom_point() +
    stat_smooth(level = 0.999, method = "loess") +
    scale_x_continuous(labels = scales::label_number(scale = 1e-3, scale_cut = scales::cut_si("s"))) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    theme_bw()

  data |>
    mutate(p_group = (n %/% 6e4)) |>
    filter(n <= 60000) |>
    mutate(p_group = (n %/% 6e3)) |>
    # filter(group != 1) |>
    #filter(group == 1) |>
    ggplot(aes(x = n, y = max, color = type)) +
    # geom_point() +
    # geom_jitter() +
    # stat_summary(aes(linetype = "mean"), fun = "mean", geom = "line") +
    #stat_summary(
    #  data = data |>
    #    filter(group == 1) |>
    #    mutate(n = 2 * 1e4 * ((n / 2) %/% 1e4)) |>
    #    summarise(max = max(max), .by = c(type, n)),
    #  aes(linetype = "max"), fun = "max", geom = "line"
    #) +
    # stat_smooth(aes(group = type),
    #   # method = "gam",
    #   method = "loess",
    #   level = 0.99,
    #   n = 60
    # ) +
    geom_boxplot(aes(group = interaction(type, p_group)), outliers = FALSE) +
    # geom_smooth() +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-3, scale_cut = scales::cut_si("s"))) +
    labs(y = "WCET", x = "Measurement Duration", color = NULL) +
    scale_color_discrete(NULL, label = \(x) stringr::str_to_title(x)) +
    # facet_grid(rows = vars(type), scales = "free_y") +
    theme_bw() +
    theme(legend.position = "right")
  ggsave(here::here("img", config$name, "box-wcet.svg"), height = 10, width = 22, units = "cm")


  data |>
    # filter(group != 1) |>
    #filter(group == 1) |>
    filter(n <=60000) |>
    mutate(n = 1e3 * (n %/% 1e3)) |>
    summarise(
      mean = mean(max),
      min = min(max),
      max = max(max),
      .by = c(type, eva, n)) |>
    ggplot(aes(x = n, color = type)) +
    geom_point(aes(y = mean, shape = is.na(eva) | (stringr::str_length(eva) == 0))) +
    geom_line(aes(linetype = "mean", y = mean), show.legend = F) +
    #geom_line(aes(linetype = "mean", y = mean)) +
    #geom_line(aes(linetype = "min", y = min))
    # geom_smooth(aes(y = max)) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-3, scale_cut = scales::cut_si("s"))) +
    scale_color_discrete(NULL, label = \(x) stringr::str_to_title(x)) +
    # facet_grid(rows = vars(type), scales = "free_y") +
    labs(title="Averaged WCET", y = "WCET", x = "Measurement Duration", color = NULL) +
    theme_bw() +
    theme(legend.position = "right")
  ggsave(here::here("img", config$name, "avg-wcet-60s.svg"), height = 10, width=22, unit="cm")

  data |>
    filter(n <= 1e4) |>
    # filter(group == 1) |>
    ggplot(aes(x = n, y = max, color = type)) +
    # geom_point() +
    # geom_jitter() +
    # stat_summary(aes(linetype = "mean"), fun = "mean", geom = "line") +
    # stat_summary(aes(linetype = "max"), fun = "max", geom = "line") +
    # stat_smooth(aes(group = type),
    #   # method = "gam",
    #   method = "loess",
    #   level = 0.99,
    #   n = 60
    # ) +
    geom_boxplot(aes(group = interaction(type, ((n / 1) %/% 1e3))), outliers = FALSE) +
    # geom_smooth() +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    scale_x_continuous(labels = scales::label_number(scale = 1e-3, scale_cut = scales::cut_si("s"))) +
    labs(y = "WCET", x = "Measurement Duration") +
    # facet_grid(rows = vars(type), scales = "free_y") +
    theme_bw() +
    theme(legend.position = "bottom")



  source(here::here("lib2/create_model.R"))
  source(here::here("lib2/transform2.R"))
  source(here::here("lib2/evaluate_method.R"))

  predict_wcet <- function(dt) {
    model <- create_model(dt, config$model)
    evaluate_method(
      N = 10000,
      config$simulation,
      Q = model$Q,
      P = model$P,
      return.type = "path"
    )
  }

  function(run, block_size, sample) {
    dt <- transitions[run <= run]

    paths <- transform2(transitions)

    max_data <- max(dt[!is.na(event.to), .(duration = sum(duration)), by = run]$duration)

    dt[!is.na(event.to), .(max = max(duration)), by = c("event.from", "event.to")]
  }

  expand.grid(
    runs = (1:600) * 1e3,
    block_sizes = 1:10,
    sampels = 10**(1:5)
  ) |> mutate(max = 1)
}
