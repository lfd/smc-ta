run_simulation <- function(start_events, end_events, P1, sample_duration) {
  take_radom_element <- function(l) sample(names(l), size = 1, replace = T, prob = l)

  if (!is.null(names(start_events))) {
    stopifnot(sum(start_events) == 1)
    state <- take_radom_element(start_events)
  } else {
    state <- sample(start_events)
  }
  rows <- list()

  while (TRUE) {
    # Exit run / path / iteration on reaching end state
    if (state %in% end_events) {
      rows[[length(rows) + 1]] <- list(event.from = state, event.to = NA, duration = NA)
      break
    }

    successor_vector <- P1[state, ]
    prev_state <- state
    state <- take_radom_element(successor_vector)

    # while (TRUE) {
    duration <- sample_duration(prev_state, state)
    # if (duration < 1) {
    #  break
    # }
    # }

    rows[[length(rows) + 1]] <- list(event.from = prev_state, event.to = state, duration = duration)
  }

  return(rbindlist(rows))
}

run_path_simulation <- function(...) {
  path <- run_simulation(...) |> # returns rows
    mutate(duration = if_else(is.na(event.to), 0, duration)) |> # end transition is NA
    summarise(
      path = paste(event.from, collapse = "→"),
      duration = sum(duration)
    )

  return(path)
}

simulate_path <- function() {
  data <- data.frame(run = numeric(), event.from = factor(), event.to = factor(), duration = numeric())
  sample_duration <- function(from, to) sampling_function(n = 1, Q[from, to, ])
  rows <- run_simulation(
    start_events,
    end_events,
    P1,
    sample_duration
  )

  # data <- rbindlist(append(rows, list(data), after = 0))
  data <- bind_rows(
    data,
    rbindlist(rows) |> mutate(run = i)
  )
}
