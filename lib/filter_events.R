filter_events <- function(dt, options = NULL) {
  stopifnot(is.data.table(dt))

  required_columns <- c("timestamp", "event")
  if (!all(required_columns %in% colnames(dt))) {
    stop("Following columns are required: ", paste(required_columns, collapse = ", "))
  }

  context <- options$context %||% character()

  # duplicate_rows <- dt[, .SD[duplicated(timestamp) | duplicated(timestamp, fromLast = TRUE)], by = "cpu"]
  stopifnot(0 == anyDuplicated(dt, by = c("timestamp", options$context)))

  tmp <- copy(dt)
  tmp[, `:=`(is_start = event %in% options$start_events, is_end = event %in% options$end_events)]

  # HACK: remove events that have same timestamp as the starting event in the same context / run
  # This is a workaround for the cyclictest data
  # TODO: make it in place; data.table does not support delete by reference
  tmp[, dup := duplicated(timestamp), by = c(context)]
  tmp <- tmp[!(dup & !is_start)]
  tmp[, dup := NULL]

  # Filter events
  # - before first start event in measurement (by context)
  # - after last end event in measurement (by context)
  # - between an end event and a start event
  tmp[, run_included := {
    purrr::accumulate2(
      .x = tmp$is_start,
      .y = tmp$is_end,
      .f = \(acc, start, end) start || (acc && !end),
      .init = FALSE
    )[-1]
  }, by = c(context)]
  tmp[, unfiltered := run_included | (is_end & shift(run_included)), by = c(context)]

  if (sum(!tmp$unfiltered) > 0) {
    tmp[, before_start := purrr::accumulate(.x = tmp$unfiltered, .f = \(acc, x) acc && !x, .init = TRUE)[-1], by = c(context)]
    tmp[, after_end := rev(purrr::accumulate(.x = rev(tmp$unfiltered), .f = \(acc, x) acc && !x, .init = TRUE)[-1]), by = c(context)]
    tmp[, between_runs := !(unfiltered | before_start | after_end)] # no grouping by context required

    logger::log_info("Filtering {sum(!tmp$unfiltered)} events")
    logger::log_info("Filtered before start: {sum(tmp$before_start)}")
    logger::log_info("Filtered after end: {sum(tmp$after_end)}")
    logger::log_info("Filtered between runs: {sum(tmp$between_runs)}")

    tmp[, `:=`(before_start = NULL, after_end = NULL, between_runs = NULL)]
  }

  tmp <- tmp[unfiltered == TRUE]

  tmp[, `:=`(run_included = NULL, unfiltered = NULL)]

  tmp[, run := as.integer(cumsum(is_start)), by = c(context)]

  tmp[, `:=`(is_start = NULL, is_end = NULL)]

  return(tmp)
}
