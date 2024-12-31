conv_markov_to_mixture <- function(df) {
  .df <- df |>
    # dplyr::mutate(censrec = if_else(is.na(event.to), 0, 1)) |>
    # dplyr::select(run, event.from, event.to, duration, censrec) |>

    # Filter legacy data
    # filter(!(state.j == state.h & time == 0)) %>%

    dplyr::select(!transition) |>
    dplyr::filter(!is.na(duration))
}

