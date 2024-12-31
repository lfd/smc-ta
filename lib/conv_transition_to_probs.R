conv_transition_to_probs <- function(df) {
  required_columns <- c("event.from", "event.to")
  stopifnot(all(required_columns %in% colnames(df)))

  .df <- df |>
    dplyr::filter(!is.na(event.to))

  stopifnot(!any(.df$event.from == .df$event.to))

  probs <- df |>
    dplyr::summarise(n = n(), .by = c("transition", "event.from", "event.to")) |>
    dplyr::mutate(probability = n / sum(n), .by = "event.from") |>
    dplyr::select(!n) |>
    dplyr::arrange(event.from, event.to)

  return(probs)
}

conv_transition_to_probs.dt<- function(dt) {
  by = c("event.from", "event.to")
  required_columns <- c(by)
  stopifnot(all(required_columns %in% colnames(dt)))

  #.dt <- dt[!is.na(event.to)]
  #stopifnot(!any(.dt$event.from == .dt$event.to))

  probs <- dt[, .(n = .N), by = .(event.from, event.to)]
  probs[, probability := n / sum(n), by = event.from]
  probs <- probs[, !"n"]
  setorder(probs, event.from, event.to)

  return(probs)
}
