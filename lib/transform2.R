transform2 <- function(dt, by = NULL) {
  by <- c("run", by)
  required_columns <- c("duration", "event.from", by)
  stopifnot(all(required_columns %in% colnames(dt)))

  dt[, .(
    path = paste(event.from, collapse = "→"),
    duration = sum(ifelse(is.na(duration), 0, duration)), # last transition (event.to=NA) has duration=NA
    length = .N
  ), by = by]
}
