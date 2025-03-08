evaluate_method <- function(df.trans, N, options, P = NULL, Q = NULL, return.type = "trans") {
  start_events <- options[["start_events"]]
  end_events <- options[["end_events"]]
  sample_fun <- options[["sample_fun"]]
  fit_fun <- options$fit_fun

  if (is.null(df.trans)) {
    abort("df.trans is NULL")
  }
  if (is.null(N)) {
    abort("N is NULL")
  }

  source(here::here("lib2", "simulate.R"))

  ## 1. Fitting
  if (is.null(Q)) {
    abort()
    stopifnot(!is.null(options$fit_fun))
    Q <- method_fit.matrix(df.trans, options$fit_fun)
    # assign("Q", Q, envir = parent.frame())
    message("Fitted Q")
  }


  if (is.null(P)) {
    abort()
    P <- df.trans |>
      as.data.table() |>
      conv_transition_to_probs.dt() |>
      format_probs_matrix(start_events, max_iter = 1) |>
      magrittr::extract2(1)

    message("Derived P")
  }

  s_fun <- function(from, to) {
    params <- Q[from, to][[1]]
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
  if (stringr::str_starts(return.type, "trans")) {
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
