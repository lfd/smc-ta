library("here")
library("rlang", exclude = c("type_of"))

NAME <- "cyclictest-ttp-ralf"
OUT_DIR <<- here("img", NAME)
DATA_DIR <<- here("..", "data", NAME)

dir.create(OUT_DIR, showWarnings = FALSE)

config <- config::get(config = NAME, use_parent = F)

PRINT <<- config$print

source("includes.R")

# save_func <- \(...) {}

data_filename <- config$data_filename %||% abort("data_filename must not be undefined")
scale_func <- match.fun(config$time_scaling) %||% abort("time_scaling cannot be NULL")
start_events <- config$start_events %||% abort("start_events cannot be NULL")
end_events <- config$end_events # if NULL => derive from data
context_types <- unlist(config$context)
context <- names(config$context)
block_size <- config$block_size %||% 10
N <- config$n_samples %||% 10000


data_load(
  filename = file.path(DATA_DIR, data_filename),
  csv_skip = config$csv_skip,
  start_events = start_events,
  end_events = end_events,
  context = context_types
)

if (!exists("trans.log")) {
  abort("Object not found")
} else if (!exists("path.log")) {
  abort("Object not found")
}

trans.log <- trans.log |>
  filter(cpu_id == 1)

path.log <- path.log |>
  filter(cpu_id == 1)


vis_probs_graph(P[[1]], save_func)

if (PRINT) {
  vis_npath(P[1:min(4, length(P))], save_func)
  vis_probs_graph(P[[1]], save_func)

  # Ensure all plot devices are closed
  while (dev.cur() != 1) {
    dev.off()
  }

  trans.log |>
    # filter(n() > 10, .by = transition) |>
    vis_transition()
}

trans.fit <- path.log


##
# Derive Block Maxima
##
if (block_size > 0) {
  source("_path-bm.R")
}




load(file = file.path(DATA_DIR, "chunk.trans.rda"))
load(file = file.path(DATA_DIR, "sampled.trans.rda"))
load(file = file.path(DATA_DIR, "df.trans.rda"))

load(file = file.path(DATA_DIR, "chunk.path.rda"))

rm(var)
#
dump_var(chunk.path, "chunk.path-low", DATA_DIR)
chunk.path_low <- chunk.path



##
# Derive Block Maxima
##

methods <- list(
  # gev = list(fit_fun = fit_gev, sample_fun = sample_gev),
  # weibull = list(fit_fun = fit_weibull, sample_fun = sample_weibull),
  mixture2 = list(fit_fun = fit_mixture2, sample_fun = sample_mixture),
  mixture3 = list(fit_fun = fit_mixture3, sample_fun = sample_mixture),
  mixture4 = list(fit_fun = fit_mixture4, sample_fun = sample_mixture)
  # mixture2 = list(fit_fun = function(...) fit_mixture(k=2, ...), sample_fun = sample_mixture),
  # mixture3 = list(fit_fun = function(...) fit_mixture(k=3, ...), sample_fun = sample_mixture),
  # mixture4 = list(fit_fun = function(...) fit_mixture(k=4, ...), sample_fun = sample_mixture)
)

total_nr_runs <- trans.log |>
  summarise(runs = max(run), .by = all_of(c(context))) |>
  summarise(runs = sum(runs)) |>
  unlist() |>
  unname()
max_runs <- min(max(trans.log$run))
sizes <- c(2**(6:log(max_runs, base = 2)), max_runs)

base4size <- 1.7
size <- trunc(c(base4size**(12:log(max_runs, base = base4size)), max_runs))
trans <- trans.bm

sizes <- 60000 * (1:10)
sizes <- 20000 * (1:30) * 3
sizes <- 2000 * (1:30) * 3

sizes <- 500 * (1:10) * 3

sizes <- c(0.1, 0.5, 1, 5, )


Qs <- purrr::pmap(
  list(sizes),
  function(size) {
    if (size > max(df.trans$run)) {
      return(NULL) # SKIP
    }

    # furrr::future_map(
    purrr::pmap(
      list(names(methods)),
      function(method) {
        message(paste("Starting fitting", method, "with n =", size))
        Q <- method_fit.matrix(
          data_select(df.trans, size),
          fit_fun = methods[[method]][["fit_fun"]],
          .simplification = TRUE
        )
        return(Q)
      }
    ) |>
      rlang::set_names(names(methods))
  }
) |>
  rlang::set_names(sizes)
dump_var(Qs, deparse(substitute(Qs)), dump_path)

library(furrr)
plan(sequential)
plan(multisession)
combinations <- expand.grid(size = sizes, method = names(methods))

trans <- trans.log
options(future.globals.maxSize = (1024 * 1024^2))

with_progress({
  p <- progressr::progressor(steps = nrow(combinations))
  sampled.trans <- future_pmap_dfr(
    combinations,
    function(size, method) {
      # if (size > max(df.trans$run)) {
      #  return(data.frame())  # SKIP
      # }
      # Progress message
      p(sprintf("Start %s for N=%d", method, size))

      # Evaluate the method
      result <- evaluate_method(
        data_select(df.trans, size, randomized = TRUE) |>
          pick_block_maxima(block_size, by = c("transition", "event.from", "event.to", context)),
        N,
        simulation_config = list(
          start_events = start_events,
          end_events = end_events,
          # P1 = P1,
          sample_fun = methods[[method]][["sample_fun"]]
        ),
        fit_fun = methods[[method]][["fit_fun"]]
      ) |>
        mutate(method = factor(method), N = size)

      return(result)
    },
    # Ensure that all necessary variables are exported to the workers
    .options = furrr_options(
      seed = TRUE,
      packages = c(
        "dplyr",
        "purrr",
        "extRemes",
        "stringi"
      ),
      globals = c(
        "methods",
        "df.trans",
        "start_events",
        "end_events",
        "p",
        "evaluate_method",
        "data_select",
        "pick_block_maxima",
        "fit_mixture"
      )
    )
  ) |>
    mutate(
      transition = as.factor(paste_trans(event.from, event.to)),
      event.from = as.factor(event.from),
      event.to = as.factor(event.to)
    )
})















with_progress({
  p <- progressr::progressor(length(sizes) * length(methods))
  sampled.trans <- purrr::map(
    sizes,
    function(size) {
      # if (size > max(df.trans$run)) {
      #  return(data.frame()) # SKIP
      # }

      # furrr::future_map(
      purrr::map(
        names(methods),
        function(method) {
          p(message = sprintf("Start %s for N=%d", method, size))
          evaluate_method(
            data_select(df.trans, size, randomized = T) |>
              pick_block_maxima(block_size, by = c("transition", "event.from", "event.to", names(context))),
            N,
            simulation_config = list(
              start_events = start_events,
              end_events = end_events,
              P1 = P1,
              sample_fun = methods[[method]][["sample_fun"]]
            ),
            fit_fun = methods[[method]][["fit_fun"]]
          ) |>
            mutate(method = factor(method))
        }
        # .env_globals = parent.frame(n = 2),
        # .options = furrr_options(seed = TRUE, prefix = as.character(size))
      ) |>
        rbindlist() |>
        mutate(N = size)
    }
  ) |>
    rbindlist() |>
    mutate(
      # origin = factor("simulation"),
      transition = as.factor(paste_trans(event.from, event.to)),
      event.from = as.factor(event.from),
      event.to = as.factor(event.to)
    )
})
dump_var(sampled.trans, deparse(substitute(sampled.trans)), DATA_DIR)

sampled.trans.col <- sampled.trans |>
  select(!c(event.from, event.to)) |>
  # mutate(i = row_number(), .by = transition) |> # if we do not have any other columns, add one
  pivot_wider(names_from = transition, values_from = duration, values_fill = NA)
# we loose N, method, etc. with this...
# sampled.path <- bind_rows(
#  data.frame(duration = pull(sampled.trans.col, `1→2`) + pull(sampled.trans.col, `2→3`), path = "1→2→3"),
#  data.frame(duration = pull(sampled.trans.col, `1→2`) + pull(sampled.trans.col, `2→4`) + pull(sampled.trans.col, `4→6`), path = "1→2→4→6"),
#  data.frame(duration = pull(sampled.trans.col, `1→2`) + pull(sampled.trans.col, `2→5`) + pull(sampled.trans.col, `5→4`) + pull(sampled.trans.col, `4→6`), path = "1→2→5→4→6"),
# ) |>
# 	filter(!is.na(duration))

sampled.path <- sampled.trans |>
  conv_transition_to_path(by = c("N", "method", "run"))
dump_var(sampled.path, "sampled.path", DATA_DIR)

chunk.trans <- bind_rows(
  sampled.trans |>
    dplyr::mutate(type = "simulated"),
  # bm.trans |>
  df.trans |>
    dplyr::mutate(
      N = Inf,
      type = "measured",
      method = "measurement"
    )
) |>
  dplyr::mutate(
    # N = as.factor(N), # better to mutate before plots
    type = as.factor(type),
    method = as.factor(method),
    transition = as.factor(transition)
  )
dump_var(chunk.trans, "chunk.trans", DATA_DIR)

# I thinkk this is not correct as df.trans in chunk.trans might be bm.tras that is not run-related / ordered
chunk.path <- chunk.trans |>
  conv_transition_to_path(by = c("N", "type", "method", "run"))


bm.path <<- pick_block_maxima(data.path, block_size, by = c("path", context))
chunk.path <- bind_rows(
  sampled.trans |>
    conv_transition_to_path(by = c("N", "method", "run")) |>
    dplyr::mutate(type = "simulated"),
  bm.path |>
    dplyr::mutate(
      N = Inf,
      type = "measured",
      method = "measurement"
    )
)
dump_var(chunk.path, "chunk.path", DATA_DIR)

bind_rows(
  filter(sampled.trans, N == max(N)) |> mutate(origin = method),
  bm.trans |> mutate(origin = "measurement")
) #|>

# PLOT LATENCY DISTRIBUTION OF TRANSITIONS
max <- max(filter(chunk.trans, is.finite(N)) |> pull(N))
chunk.trans |>
  mutate(origin = method) |>
  filter(origin == "measurement" | N == max) |>
  filter(!is.na(duration)) |>
  ggplot(aes(color = origin)) +
  stat_ecdf(aes(duration), geom = "line", linetype = "dotted") +
  stat_density(aes(duration, after_stat(scaled)), geom = "line", position = "identity") +
  scale_y_sqrt() +
  scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
  stat_summary(aes(x = duration, y = 1, xintercept = after_stat(x), linetype = "95%"), geom = "vline", orientation = "y", fun = "quantile", fun.args = list(probs = c(.95))) +
  stat_summary(aes(x = duration, y = 1, xintercept = after_stat(x), linetype = "99%"), geom = "vline", orientation = "y", fun = "quantile", fun.args = list(probs = c(.99))) +
  stat_summary(aes(x = duration, y = 1, xintercept = after_stat(x), linetype = "99.9%"), geom = "vline", orientation = "y", fun = "quantile", fun.args = list(probs = c(.999))) +
  facet_wrap(vars(transition), scales = "free")


max <- max(filter(chunk.path, is.finite(N)) |> pull(N))
chunk.path |>
  mutate(origin = method) |>
  # filter(origin == "measurement" | N == max) |>
  ggplot(aes(color = origin)) +
  stat_ecdf(aes(duration), geom = "line", linetype = "dotted") +
  stat_density(aes(duration, after_stat(scaled)), geom = "line", position = "identity") +
  scale_y_sqrt() +
  scale_x_continuous(limits = c(0, NA), labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
  stat_summary(aes(x = duration, y = 1, xintercept = after_stat(x), linetype = "95%"), geom = "vline", orientation = "y", fun = "quantile", fun.args = list(probs = c(.95))) +
  stat_summary(aes(x = duration, y = 1, xintercept = after_stat(x), linetype = "99%"), geom = "vline", orientation = "y", fun = "quantile", fun.args = list(probs = c(.99))) +
  stat_summary(aes(x = duration, y = 1, xintercept = after_stat(x), linetype = "99.9%"), geom = "vline", orientation = "y", fun = "quantile", fun.args = list(probs = c(.999))) +
  # facet_wrap(vars(path), scales = "free") +
  facet_wrap(vars(paste(N / 1000 / 60 / 3, "min"))) +
  labs(linetype = "Quantile")

by_quantiles <- chunk.path |>
  # filter(method == "mixture3") |>
  filter(method != "measurement") |>
  # mutate(N = N / 60000) |>
  mutate(N = as.factor(N)) |>
  calc_quantiles(by = c("method", "N"), probs = c(.999, .9999, .99999, .999999))
chunk.path |>
  mutate(N = as.factor(N)) |>
  filter(method == "mixture3") |>
  ggplot(aes(x = duration, y = N)) +
  # stat_density_ridges(alpha = 0) +
  geom_segment(
    data = by_quantiles,
    aes(
      # linetype = paste0(name * 100, "%"),
      # linetype = method,
      color = paste0(name * 100, "%"),
      x = value,
      xend = value,
      y = as.integer(N),
      yend = as.integer(N) + .9,
      # y = as.numeric(factor(.data[["N"]])),
      # yend = as.numeric(factor(.data[["N"]])) + .9,
      # color = factor(.data[[comparison]])
    )
  ) +
  facet_grid(rows = vars(method)) +
  scale_x_continuous("Latency", labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
  labs(y = "Duration of Measurement [min]", color = "Quantile")

probs <- 1 - (10^(-(1:10)))
by_quantiles <- chunk.path |>
  filter(method != "gev") |>
  calc_quantiles(by = c("method", "N"), probs = probs) #|>
mutate(name = scales::label_percent()(name))
by_quantiles |>
  filter(method != "measurement") |>
  ggplot(aes(x = N, y = value, color = method)) +
  geom_point() +
  geom_line() +
  geom_hline(data = filter(by_quantiles, method == "measurement"), aes(yintercept = value)) +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
  facet_wrap(~name, scale = "free_y") +
  labs(y = "Latency", x = "Duration of Measurement")

by_quantiles |>
  filter(method != "measurement") |>
  ggplot(aes(x = N, y = value, color = name)) +
  geom_point() +
  geom_line() +
  geom_hline(data = filter(by_quantiles, method == "measurement") |> select(-method), aes(yintercept = value, color = name)) +
  scale_y_continuous(label = scales::label_number(scale_cut = scales::cut_si("s"))) +
  facet_wrap(~method)

by_quantiles |>
  mutate(N = N / 1000 / 60 / 3) |>
  filter(method != "measurement" & method == "mixture3") |>
  ggplot(aes(x = N, y = value, color = method)) +
  geom_point() +
  geom_line() +
  geom_hline(data = filter(by_quantiles, method == "measurement"), aes(yintercept = value)) +
  scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
  # scale_x_continuous(breaks = seq(0, 600, by = 60), labels = function(x) paste(x / 60, "min")) +
  scale_x_continuous(labels = \(x) paste(x, "min")) +
  facet_wrap(~name)


by_quantiles |>
  mutate(N = N / 1000 / 60 / 3) |>
  filter(name >= .999 & name <= .99999) |>
  mutate(name = as.factor(scales::label_percent()(name))) %>%
  {
    . |>
      filter(method != "measurement" & method == "mixture3") |>
      ggplot(aes(x = N, y = value, color = name)) +
      geom_point() +
      geom_line() +
      geom_hline(data = filter(., method == "measurement"), linetype = "dashed", aes(color = name, yintercept = value)) +
      scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
      # scale_x_continuous(breaks = seq(0, 600, by = 60), labels = function(x) paste(x / 60, "min")) +
      scale_x_continuous(labels = \(x) paste(x, "min")) +
      labs(y = "Latency", x = "Duration of Measurement", color = "Quantile")
  }

by_extrems <- chunk.path |>
  calc_extrems(by = c("method", "N")) |>
  filter(method != "gev")
by_extrems %>%
  filter(name == "max") |>
  filter(!(name %in% c("median", "sd"))) %>%
  {
    . |>
      filter(method != "measurement") |>
      ggplot(aes(x = N, y = value, color = method, linetype = name)) +
      geom_hline(data = filter(., method == "measurement"), aes(linetype = name, yintercept = value)) +
      scale_x_continuous(trans = "log2", breaks = scales::trans_breaks("log2", function(x) 2^x), labels = scales::trans_format("log2", scales::math_format(2^.x))) +
      scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
      geom_point() +
      geom_line()
  }
by_extrems %>%
  filter(name == "max") %>%
  {
    . |>
      filter(method != "measurement") |>
      mutate(method = stringr::str_replace(method, "mixture", "Method ")) |>
      ggplot(aes(x = N, y = value, color = method)) +
      geom_hline(data = filter(., method == "measurement"), aes(color = NULL, yintercept = value)) +
      scale_x_continuous(trans = "log2", breaks = scales::trans_breaks("log2", function(x) 2^x), labels = scales::trans_format("log2", scales::math_format(2^.x))) +
      scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
      geom_point() +
      geom_line() +
      labs(color = "Source", y = "WCET", x = "Number of Runs / Measurement")
  }

by_extrems %>%
  filter(name == "max") %>%
  {
    . |>
      filter(method != "measurement") |>
      ggplot(aes(x = N, y = value, color = method)) +
      geom_hline(data = filter(., method == "measurement"), aes(yintercept = value)) +
      scale_x_continuous(trans = "log2", breaks = scales::trans_breaks("log2", function(x) 2^x), labels = scales::trans_format("log2", scales::math_format(2^.x))) +
      scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
      geom_point() +
      geom_line() +
      labs(y = "Max. Latency", x = "Nr. of Runs")
  }
by_extrems %>%
  mutate(N = N / 1000 / 3) |>
  filter(name == "max") %>%
  {
    . |>
      filter(method != "measurement") |>
      filter(method == "mixture3") %>%
      mutate(method = stringr::str_replace(method, "mixture", "Method ")) |>
      ggplot(aes(x = N, y = value, color = method)) +
      geom_hline(data = filter(., method == "measurement"), aes(yintercept = value)) +
      # scale_x_continuous(trans = "log2", breaks = scales::trans_breaks("log2", function(x) 2^x), labels = scales::trans_format("log2", scales::math_format(2^.x))) +
      scale_y_log10(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
      scale_x_time() +
      # scale_x_continuous(breaks = seq(0, 600, by = 60), labels = function(x) paste(x / 60, "min")) +
      geom_point() +
      geom_line() +
      guides(color = "none") +
      labs(y = "Max. Latency", x = "Duration of Measurement", color = "Estimation")
  }

nl <- 0

nlp <- 3 * 1000 * 20 * 3

nl <- nl + nlp

chunk.path |>
  mutate(origin = method) |>
  filter(N == nl) |>
  filter(origin == "mixture3") |>
  mutate(origin = "Estimation") |>
  bind_rows(
    bm.path |> mutate(origin = "Measurements")
  ) |>
  ggplot(aes(color = origin)) +
  stat_bin(aes(duration, y = after_stat(ndensity)), alpha = 0, position = "identity") +
  # stat_bin(data = bm.path, aes(duration, y = after_stat(ndensity)), alpha = 0) +
  # stat_summary(aes(x = duration, y = 1, xintercept = after_stat(x), linetype = "99.999%"), geom = "vline", orientation = "y", fun = "quantile", fun.args = list(probs = c(.99999))) +
  stat_summary(aes(x = duration, y = 1, xintercept = after_stat(x), linetype = "Maximum"), geom = "vline", orientation = "y", fun = "max") +
  scale_y_sqrt() +
  scale_x_continuous(
    limits = c(14e-6, NA),
    labels = scales::label_number(scale_cut = scales::cut_si("s"))
  ) +
  labs(
    title = paste("Measured", nl / 1000 / 3, "seconds"),
    x = "Latencies",
    y = "Occurrences",
    color = "",
    linetype = ""
  )

bind_rows(
  data.path |> data_select(3 * 20000, c(context), randomized = T) |> mutate(origin = ""),
  # bm.path |> mutate(origin = "Measurements")
) |>
  ggplot(aes(color = origin)) +
  stat_bin(aes(duration, y = after_stat(ndensity)), alpha = 0, position = "identity") +
  # stat_bin(data = bm.path, aes(duration, y = after_stat(ndensity)), alpha = 0) +
  # stat_summary(aes(x = duration, y = 1, xintercept = after_stat(x), linetype = "99.999%"), geom = "vline", orientation = "y", fun = "quantile", fun.args = list(probs = c(.99999))) +
  stat_summary(aes(x = duration, y = 1, xintercept = after_stat(x), linetype = "Maximum"), geom = "vline", orientation = "y", fun = "max") +
  scale_y_sqrt() +
  scale_x_continuous(
    limits = c(14e-6, NA),
    labels = scales::label_number(scale_cut = scales::cut_si("s"))
  ) +
  labs(
    title = paste("Measured 60 seconds"),
    x = "Latencies",
    y = "Occurrences",
    color = "",
    linetype = ""
  ) +
  guides(linetype = "none", color = "none")


bind_rows(
  chunk.path |> filter(N == 180000 & method == "mixture3") |> mutate(origin = method),
  # bm.path |> mutate(origin = "Measurements")
) |>
  ggplot(aes(color = origin)) +
  stat_bin(aes(duration, y = after_stat(ndensity)), alpha = 0, position = "identity") +
  # stat_bin(data = bm.path, aes(duration, y = after_stat(ndensity)), alpha = 0) +
  # stat_summary(aes(x = duration, y = 1, xintercept = after_stat(x), linetype = "99.999%"), geom = "vline", orientation = "y", fun = "quantile", fun.args = list(probs = c(.99999))) +
  scale_x_continuous(
    limits = c(14e-6, NA),
    labels = scales::label_number(scale_cut = scales::cut_si("s"))
  ) +
  theme_minimal() +
  labs(
    x = "",
    y = "",
    color = "",
    linetype = ""
  ) +
  guides(linetype = "none", color = "none")






# process_data(filename, start_events, match.fun(config$time_scaling), save_func)
