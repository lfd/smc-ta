#conv_transition_to_markov <- function(df) {
#  markov <- df |>
#    dplyr::mutate(censrec = if_else(is.na(event.to), 0, 1)) |>
#    dplyr::select(run, event.from, event.to, duration, censrec)
#
#  return(markov)
#}

# conv_transition_to_markov <- function(df) {
#   df %>%
#     dplyr::mutate(
#       # Order matters!! id, state.h, state,j, time
#       id = run,
#       state.h = event.from,
#       state.j = event.to,
#       time = duration,
#       .keep = "none"
#     ) %>%
#     # Prepare right-cencored / absorbing states
#     dplyr::mutate(
#       time = if_else(is.na(state.j), 0, time),
#       state.j = if_else(is.na(state.j), state.h, state.j),
#       censrec = if_else(state.j == state.h, 0, 1),
#     )
# }
