require("here")
OUT_DIR <- here("img", "cyclictest-bpf")
PRINT <- FALSE
source("includes.R")

scale_func <- function(df, ...) {
  # Scaling (ns -> s)
  mutate(df, duration = duration * 10e-9, ...)
}

start_events <- c("start")
end_events <- c("stop")
filename <- here("events.csv")
context <- c(tid = "f")
csv_skip <- 1

data_load(
  filename = filename,
  csv_skip = csv_skip,
  start_events = start_events,
  end_events = end_events,
  context = context
)

save_func <- \(...) {}


if (PRINT) {
  vis_npath(P[1:min(4,length(P))], save_func)
  vis_probs_graph(P[[1]], save_func)

  # Ensure all plot devices are closed
  while (dev.cur() != 1) {
    dev.off()
  }

  data.trans |>
    # filter(n() > 10, .by = transition) |>
    vis_transition()
}

# Simplify data
# CAUTION!!!!
data.trans <- data.trans |>
  filter(n() >= 100 * block_size, .by = transition)

block_size <- 10 # You can adjust the block size as needed
##
# Derive Block Maxima
##
bm.trans <<- pick_block_maxima(data.trans, block_size, by = c("transition", "event.from", "event.to", names(context)))
bm.path <<- pick_block_maxima(data.path, block_size, by = c("path"))

methods <- list(
  gev = list(fit_fun = fit_gev, sample_fun = sample_gev),
  # weibull = list(fit_fun = fit_weibull, sample_fun = sample_weibull),
  mixture2 = list(fit_fun = fit_mixture2, sample_fun = sample_mixture),
  mixture3 = list(fit_fun = fit_mixture3, sample_fun = sample_mixture),
  mixture4 = list(fit_fun = fit_mixture4, sample_fun = sample_mixture)
  # mixture2 = list(fit_fun = function(...) fit_mixture(k=2, ...), sample_fun = sample_mixture),
  # mixture3 = list(fit_fun = function(...) fit_mixture(k=3, ...), sample_fun = sample_mixture),
  # mixture4 = list(fit_fun = function(...) fit_mixture(k=4, ...), sample_fun = sample_mixture)
)

df.trans <- bm.trans
N <- 10000
sizes <- 2**(6:log(max(data.log$run), base = 2))

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

P1 <- P[[1]]

# requires following packages remotely
# - extRemes
# - stringi
with_progress({
  p <- progressr::progressor(length(sizes) * length(methods))
  sampled.trans <- map(
    sizes,
    function(size) {
      if (size > max(df.trans$run)) {
        return(data.frame()) # SKIP
      }

      # furrr::future_map(
      purrr::map(
        names(methods),
        function(method) {
          p(message = sprintf("Start %s for N=%d", method, size))
          evaluate_method(
            data_select(df.trans, size),
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

chunk.trans <- bind_rows(
  sampled.trans |>
    dplyr::mutate(type = "simulated"),
  # bm.trans |>
  data.trans |>
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

chunk.path <- chunk.trans |>
  conv_transition_to_path(by = c("N", "type", "method", "run"))


bind_rows(
  filter(sampled.trans, N == max(N)) |> mutate(origin = method),
  bm.trans |> mutate(origin = "measurement")
) |>
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


process_data(filename, start_events, scale_func, save_func)
