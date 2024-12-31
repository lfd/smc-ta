evaluate_method <- function(df.trans, N, simulation_config, fit_fun = NULL, Q = NULL, return.type = "trans") {
  start_events <- simulation_config[["start_events"]]
  end_events <- simulation_config[["end_events"]]
  P1 <- simulation_config[["P1"]]
  sample_fun <- simulation_config[["sample_fun"]]

  if (is.null(df.trans)) {
    abort("df.trans is NULL")
  }
  if (is.null(N)) {
    abort("N is NULL")
  }


  ## 1. Fitting
  if (is.null(Q)) {
    stopifnot(!is.null(fit_fun))
    Q <- method_fit.matrix(df.trans, fit_fun)
    # assign("Q", Q, envir = parent.frame())
    message("Fitted Q")
  }


  P <- df.trans |>
    as.data.table() |>
    conv_transition_to_probs.dt() |>
    format_probs_matrix(start_events, max_iter = 1) |>
    magrittr::extract2(1)

  message("Derived P")

  # P1 - P

  s_fun <- function(from, to) {
    params <- NULL
    if (is.matrix(Q)) {
      params <- Q[from, to, ]
    } else {
      params <- purrr::pluck(Q, "params", which(Q$event.from == from & Q$event.to == to))
    }
    sampling_possible <- !(
      is.null(params) || (purrr::is_list(params) && purrr::list_c(params) %>%
        {
          any(is.null(.)) || any(is.na(.))
        })
    )
    if (sampling_possible) {
      sample <- sample_fun(params = params)
    } else {
      warn("Sampling was not possible")
      sample <- df.trans |>
        filter(event.from == from & event.to == to) |>
        pull(duration) |>
        sample(1)
    }
    return(sample)
  }

  simulator <- NULL
  if (return.type == "trans" || return.type == "transition") {
    simulator <- run_simulation
  } else if (return.type == "path") {
    simulator <- run_path_simulation
  } else {
    abort("invalid return.type")
  }

  message("Start Simulation")

  ## 2. Sampling
  purrr::map(
    seq(1, N),
    function(i) {
      i <- unlist(i)
      if (i %% (N %/% 100) == 0) {
        message(paste("run", i))
      }
      simulator(start_events, end_events, P, s_fun) |>
        mutate(run = i)
    }
  ) |>
    rbindlist()
}
