library(rlang)
library(data.table)
library(logger)

# ... siehe main.R

config <- readRDS("TMP-config.rds")
source.data <- readRDS("TMP-data.rds")

portion <- "n2"
ns <- seq(1e3, 10e3, 1e3)
ns <- seq(1)

start.time <- Sys.time()

library(future)
library(dplyr)
NPROC <- 12
future::plan(multicore, workers = NPROC)

data <- purrr::map(
  ns,
  function(n) {
    furrr::future_map(
      seq(1, 24),
      function(model_idx) {
        source(here::here("lib", "create_model.R"))
        source(here::here("lib", "evaluate_method.R"))
        N <- 10000
        t <- source.data |>
          filter(run <= n)
        probs <- c(.75, .8, .85, .9, .95, .99, .999, .9999, .99999, 1)
        model <- create_model(t, options = config$model)
        inform(glue::glue("Model {model_idx} fitted"))

        purrr::map(
          seq(1, 10),
          function(run_idx) {
            qs <- t |>
              evaluate_method(
                N = N,
                P = model$P,
                Q = model$Q,
                options = config$simulation,
                return.type = "path"
              ) |>
              dplyr::pull(duration) |>
              quantile(probs = probs, names = F)

            list(
              N = N,
              n = n,
              model = model_idx,
              run = run_idx
            ) |> append(
              setNames(qs, probs)
            )
          }
        ) |> rbindlist()
      }
    ) |>
      rbindlist()
  }
) |> rbindlist()
end.time <- Sys.time()

saveRDS(data, here::here(glue::glue("data/{config$name}/batch-stability_{portion}.rds")))

diff <- end.time - start.time

print(glue::glue("Model Evaluation Time: Start={start.time} - End={end.time} = {diff}"))

quit()

###############################################


source(here::here("lib", "transform2.R"))
library("ggplot2")
is4paper <- T
meas_data <- source.data |> transform2(config$data$context)

if (is4paper) {
  COLOURS.LIST <- c("black", "#E69F00", "#999999", "#009371")

  BASE.SIZE <- 9 # IEEEtrans uses 10 by deault

  theme_paper_base <- function() {
    return(theme_bw(base_size = BASE.SIZE) +
      theme(
        axis.title.x = element_text(size = BASE.SIZE),
        axis.title.y = element_text(size = BASE.SIZE),
        legend.title = element_text(size = BASE.SIZE),
        # legend.position = "top",
        plot.margin = unit(c(0, 0, 0, 0), "cm"),
      ))
  }
}

portion <- "all"
data <- readRDS(here::here(glue::glue("data/{config$name}/batch-stability_{portion}.rds"))) |>
  tidyr::pivot_longer(!c("N", "model", "run"), names_to = "quantile")

{
  source(here::here("lib", "transform2.R"))
  probs <- c(.75, .8, .85, .9, .95, .99, .999, .9999, .99999, 1)
  data_both <- rbindlist(list(
    data |>
      mutate(q = as.numeric(quantile)) |>
      mutate(source = "model"),
    meas_data |>
      reframe(
        value = quantile(duration, probs, names = F),
        q = probs
      ) |>
      mutate(source = "measurement")
  ), fill = T) |>
    mutate(q_name = ordered(q, labels = unique(scales::label_percent(0.001)(q)))) #|>
  # mutate(q_name = forcats::fct_relabel(q_name, \(x) ifelse(x == scales::label_percent(0.001)(1), "Max", x)))

  filter(data_both) |>
    summarise(max = mean(value), .by = c("source", "q")) |>
    tidyr::pivot_wider(names_from = source, values_from = max) |>
    mutate(diff = model - measurement, diff_rel = scales::label_percent()(diff / measurement))

  plot <- data_both %>%
    {
      if (is4paper) {
        filter(., q > 0.95)
      } else {
        .
      }
    } %>%
    # This aggregates all Samples per Model
    summarise(value = mean(value), .by = c("N", "model", "q", "source", "q_name")) |>
    ggplot(aes(x = q_name, y = value, color = source)) +
    geom_jitter(data = . %>% filter(source == "model"), height = 0, color = COLOURS.LIST[[3]]) +
    geom_boxplot(data = . %>% filter(source == "model"), outliers = F) +
    geom_point(data = . %>% filter(source == "measurement") %>% summarise(value = mean(value), .by = c("q", "q_name", "source")), aes(y = value, color = source), size = 2) +
    geom_line(data = . %>% filter(source == "measurement") %>% summarise(value = mean(value), .by = c("q", "q_name", "source")), aes(y = value, color = source, group = source)) +
    scale_y_continuous("Execution Time [µs]",
      # limits = c(0, NA),
      labels = \(x) x * 1e6,
      # labels = scales::label_number(scale_cut = scales::cut_si("s"))
    ) +
    labs(x = "Quantile Level") +
    scale_color_manual(NULL, values = COLOURS.LIST, label = \(x) stringr::str_to_title(x)) +
    theme_paper_base() +
    guides(color = guide_legend(position = "inside")) +
    theme(legend.position = "bottom") +
    theme(
      axis.text.x = element_text(angle = 20, hjust = 0.66),
      legend.justification.inside = c(0.05, 0.95),
    )
  print(plot)

  if (is4paper) {
    plot
    f_name <- "results"
    ggsave(
      here::here("img", "paper", config$name, "results.tikz"),
      device = tikzDevice::tikz,
      sanitize = T,
      #here::here("img", "paper", config$name, "results.png"),
      width = 8.6 * 1,
      height = 8,
      unit = "cm"
    )
  }
}


portion <- "n"
data <- readRDS(here::here(glue::glue("data/{config$name}/batch-stability_{portion}.rds"))) |>
  tidyr::pivot_longer(!c("N", "n", "model", "run"), names_to = "quantile")

{
  ns <- unique(data$n)
  meas_results <-
    purrr::map(
      ns,
      function(n) {
        meas_data |>
          filter(run <= n) |>
          reframe(
            value = quantile(duration, probs, names = F),
            q = probs,
            n = n
          )
      }
    ) |>
    rbindlist() |>
    mutate(source = "measurement") |>
    mutate(q_name = ordered(q, labels = unique(scales::label_percent(0.001)(q)))) #|>
  # mutate(q_name = forcats::fct_relabel(q_name, \(x) ifelse(x == scales::label_percent(0.001)(1), "Max", x)))

  q_filter <- function(.data) {
    if (is4paper) {
      filter(.data, q %in% c(0.95, 0.999, 0.9999, 1)) |>
        mutate(q_name = ordered(q, labels = unique(scales::label_percent(0.001)(q))))
    } else {
      .data
    }
  }

  plot <- data_both |>
    q_filter() |>
    filter(source == "model") |>
    # This aggregates all Samples per Model
    summarise(value = mean(value), .by = c("N", "n", "model", "q", "source", "q_name")) |>
    ggplot(aes(x = n, y = value)) +
    geom_boxplot(aes(group = n, color = "1_model"), outliers = F) +
    #geom_jitter(size = 1, height = 0, color = COLOURS.LIST[[3]]) +
    geom_hline(data = filter(q_filter(data_both), source == "measurement"), aes(yintercept = value, color = "2_all"), linewidth = 1) +
    geom_point(data = q_filter(meas_results), aes(color = "3_portion"), size = 2) +
    facet_wrap(c("q_name"), ncol = (if (is4paper) 2 else 3), scales = "free_y") +
    scale_y_continuous("Execution Time [µs]", labels = \(x) x * 1e6) +
    scale_x_continuous("Duration of Measurement [s]", labels = scales::label_number(accuracy = 1, scale = 1e-3), breaks = ns) +
    scale_color_manual(
      "Quantile Level of",
      values = c("1_model" = COLOURS.LIST[[1]], "3_portion" = COLOURS.LIST[[2]], "2_all" = COLOURS.LIST[[4]]),
      labels = c("1_model" = "Model Estimations", "3_portion" = "Portion of Measurements", "2_all" = "All Measurements"),
      aesthetics = c("colour", "fill")
    ) +
    labs(color = NULL) +
    guides(
      # color = guide_legend(position = "inside")
      color = guide_legend(position = "bottom")
    ) +
    theme_paper_base() +
    theme(legend.justification.inside = c(1, 0))
  print(plot)

  if (is4paper) {
    plot
    # f_name <- "tradeoff"
    f_name <- "tradeoff-modelAgg"
    ggsave(
      # here::here("img", "paper", config$name, paste0(f_name, ".tikz")),
      # device = tikzDevice::tikz,
      # sanitize = T,
      here::here("img", "paper", config$name, paste0(f_name, ".png")),
      width = 8.6 * 2,
      height = 10,
      unit = "cm"
    )
  }
}
