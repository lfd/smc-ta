conv_transition_to_path <- function(df, by = c("run")) {
  required_columns <- c("duration", "event.from", by)
  stopifnot(all(required_columns %in% colnames(df)))

  paths <- df |>
    dplyr::summarise(
      path = paste(event.from, collapse = "→"),
      duration = sum(tidyr::replace_na(duration, 0)), # last transition (event.to=NA) has duration=NA
      length = n(),
      .by = all_of(by)
    )

  return(paths)
}

conv_transition_to_path.dt <- function(df, by = c("run")) {
  required_columns <- c("duration", "event.from", by)
  stopifnot(all(required_columns %in% colnames(df)))

  paths <- df[, .(
    path = paste(event.from, collapse = "→"),
    duration = sum(fifelse(is.na(duration), 0, duration)), # last transition (event.to=NA) has duration=NA
    length = .N
  ), by = by]

  return(paths)
}
