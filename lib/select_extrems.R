select_extrems <- function(dt, options = NULL) {
  method <- options$method

  if (is.null(method)) {
  	return(dt[!is.na(duration)])
  }

  if (method == "bmm") {
  	select_block_maxima(dt, options$block_size)
  } else if (method == "pot") {
  	abort()
  	select_peak_over_threshold(dt, options$threshold)
  } else {
  	abort("Unkown extreme value selection method")
  }
}

select_over_threshold <- function(dt, thresholds) {
  stopifnot(is.data.table(dt))

  required_columns <- c("duration")
  if (!all(required_columns %in% colnames(dt))) {
    stop("Following columns are required: ", paste(required_columns, collapse = ", "))
  }

	dt[duration > threshold]
}

select_block_maxima <- function(dt, block_size, by = NULL) {
  stopifnot(is.data.table(dt))

  by <- c("event.from", "event.to", by) |> unique()
  required_columns <- c(by, "duration")
  if (!all(required_columns %in% colnames(dt))) {
    stop("Following columns are required: ", paste(required_columns, collapse = ", "))
  }

  dt <- dt[!is.na(duration)]
  dt[, block_id := (seq_len(.N) - 1) %/% block_size, by = by]
  dt[, .SD[head(which.max(duration), 1)], by = c("block_id", by)]
}
