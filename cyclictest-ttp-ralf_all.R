source("cyclictest-ttp-ralf.R")

start_events <- config$start_events %||% abort("start_events cannot be NULL")
end_events <- config$end_events # if NULL => derive from data
context_types <- unlist(config$context)
context <- names(context_types)
block_size <- config$block_size %||% 10
N <- config$n_samples %||% 10000

data_load_cached("trans.log")
rm(var)

if (!exists("trans.log")) {
  abort("Object not found")
}

trans.log <- trans.log |>
  filter(cpu_id == 1)

trans.bm <<- pick_block_maxima(trans.log, block_size, by = c("event.from", "event.to", context))
trans.fit <- trans.bm



#########################################

{
  method <- list(fit_fun = fit_mixture4, sample_fun = sample_mixture)
  Q <- method_fit.matrix(
    trans.fit,
    fit_fun = method[["fit_fun"]],
    .simplification = TRUE
  )
  plan(multisession) # Use multiple sessions for parallel processing
  trans.est <- evaluate_method(
    trans.fit,
    N,
    simulation_config = list(
      start_events = start_events,
      end_events = end_events,
      # P1 = P1,
      sample_fun = method[["sample_fun"]]
    ),
    fit_fun = method[["fit_fun"]],
    Q = Q
  )
  plan(sequential)

  trans <- rbind(
    trans.est |>
      mutate(method = "estimation"),
    trans.fit |>
      select(event.from, event.to, duration, run) |>
      mutate(method = "measurement")
  )

  path.est <- trans.est |>
  	conv_transition_to_path.dt(by = c("run")) |>
  	select(!length)

data_load_cached("path.log")
path.log <- path.log |> select(!length)

  paths <- rbind(
  	as.data.frame(path.est) |>
  		mutate(cpu_id = NA) |>
  		mutate(method = "estimate") ,
  	 path.log |>
  	#pick_block_maxima(block_size, by = c(context)) |>
  		mutate(method = "measurement")
  )

  lookup[filter(paths, method == "estimate")$path |> unique()]|> names()
  lookup[filter(paths, method == "measurement")$path |> unique()] |> names()

  path_names <- unique(paths$path)
  lookup <- seq(1, length(path_names))
  names(lookup) <- path_names
  lookup
  paths$path <- unname(lookup[paths$path])

  quantiles <- calc_quantiles(paths, by = c("method"), 1 - (10 ^ -seq(4,9))) |>
  	mutate(name = factor(name))
  extrems <- calc_extrems(paths, by = c("method"))
  maxes <- filter(extrems, name == "max")

  paths |>
  	filter(n() > 10, .by = c(path)) |>
  	ggplot(aes(x = duration, color = method)) +
  	stat_density(aes(y = after_stat(ndensity)), geom = "line", position = "identity") +
  	scale_x_continuous(limits = c(25e-6, 120e-6)) +
  	geom_jitter(data = quantiles, aes(x = value, y = .5, color = method, shape = name), width = F, height = T)+
  	geom_vline(data = maxes, aes(xintercept = value, color = method), width = 2)
}


#########################################


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

Qs <- purrr::pmap(
  list(names(methods)),
  function(method) {
    message(paste("Starting fitting", method))
    Q <- method_fit.matrix(
      trans.fit,
      fit_fun = methods[[method]][["fit_fun"]],
      .simplification = TRUE
    )
    return(Q)
  }
) |>
  rlang::set_names(names(methods))

plan(multisession) # Use multiple sessions for parallel processing

result <- purrr::pmap(
  list(names(methods)),
  function(method) {
    p(sprintf("Start %s", method))
    evaluate_method(
      trans.fit,
      N,
      simulation_config = list(
        start_events = start_events,
        end_events = end_events,
        # P1 = P1,
        sample_fun = methods[[method]][["sample_fun"]]
      ),
      fit_fun = methods[[method]][["fit_fun"]],
      Q = Qs[[method]]
    ) |>
      mutate(method = factor(method))
  }
)
names(result) <- names(methods)

# Reset the parallel plan to avoid potential side effects
plan(sequential)

{
  trans <- rbindlist(result) |>
    filter(!is.na(duration)) |> # Remove end_event → NA
    bind_rows(trans.fit |> mutate(method = "Raw Data")) |>
    mutate(
      transition = paste_trans(event.from, event.to),
      method = as.factor(method)
    )
  quantiles <- trans |> calc_quantiles(c("transition", "method"), c(.999, .9999))
  maxes <- trans |> summarise(value = max(duration), .by = all_of(c("transition", "method")))
  trans |>
    ggplot(aes(x = duration)) +
    scale_x_continuous("Transition Duration [µs]", labels = function(x) x * 1e6) +
    facet_wrap(
      # cols = vars(method),
      # rows = vars(transition),

      vars(transition),
      ncol = 3,
      scales = "free_x"
    ) +
    stat_density_ridges(aes(y = method, color = method), show.legend = F) +
    geom_segment(data = maxes, aes(x = value, xend = value, y = 0, yend = Inf, color = method)) +
    geom_jitter(data = quantiles, aes(x = value, y = method, color = method, shape = as.character(name)), width = 0, height = .2, size = 3) +
    guides(color = "none") +
    scale_shape_discrete() +
    labs(shape = "Quantile", y = NULL)
  save_for_thesis("transitions-fitted-all-ridges", full_page = T)
}
