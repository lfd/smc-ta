prepare <- function(dt, options, with.filtering = T) {
  if (!is.data.table(dt)) {
    stop("Input must be a data.table")
  }

  required_columns <- c("timestamp", "event")
  if (!all(required_columns %in% colnames(dt))) {
    stop("Following columns are required: ", paste(required_columns, collapse = ", "))
  }

  start_events <- options$start_events
  end_events <- options$end_events
  context <- options$context

  filter_events_after_end.dt <- function(dt) {
    dt_filtered <- dt[run == mask]
    skip_count <- nrow(dt) - nrow(dt_filtered)
    if (skip_count > 0) {
      rlang::inform(
        R.utils::printf("Skip %s events occuring after runs's end event", skip_count |> scales::number(big.mark = ",")),
        body = dt[run != mask, .(count = .N), by = "event"] |> as_tibble() |> format()
      )
    }
    return(dt_filtered[, with = FALSE])
  }

  filter_events_before_start.dt <- function(dt) {
    dt_filtered <- dt[run != 0]
    skip_count <- nrow(dt) - nrow(dt_filtered)
    if (skip_count > 0) {
      rlang::inform(
        R.utils::printf("Skip %s events occuring before any start event", skip_count |> scales::number(big.mark = ",")),
        body = dt[run == 0, .(count = .N), by = "event"] |> as_tibble() |> format()
      )
    }
    return(dt_filtered)
  }

  filter_tailing_events.dt <- function(dt) {
    # FIXME: must be done per context

    last_run <- max(dt$run)
    last_run_events <- dt[, run == max(run), by = context]
    if (!any(last_run_events$event %in% end_events)) {
    } else {
      rlang::inform(
        R.utils::printf("Skip incomplete run with %s events at the end of the dataset", count |> scales::number(big.mark = ",")),
        body = "" # TODO
      )

      dt <- dt[run != last_run, ]
    }
    return(dt)
  }

  ## Stop if there are events within the same context with duplicate timestamps
  # result <- dt[, .(duplicated = any(duplicated(timestamp))), by = context]
  # if (any(result$duplicated)) {
  #  enforce_continuation <- F
  #  errors <- filter(dt, n() > 1, .by = all_of(c(context, "timestamp")))
  #  browser()
  #  if (!enforce_continuation) {
  #    stop()
  #  }
  #  enforce_continuation <- T
  # }

  setorder(dt, timestamp) # should have been set, but just to be sure.

  dt[, starting := (event %in% start_events), by = context]

  # HACK: remove events that have same timestamp as the starting event in the same context / run
  # This is a workaround for the cyclictest data
  dt[, dup := duplicated(timestamp), by = context]
  #dt <- dt[(!dup | starting), !c("dup")]
  dt <- dt[!(dup & !starting)][, dup := NULL]

  dt[, run := as.integer(cumsum(starting)), by = context]
  dt[, starting := NULL]


  # FILTER EVENTS OF NO INTEREST
  dt[, mask := 1 + cumsum(shift(event, type = "lag") %in% end_events), by = context]
  if (with.filtering){
    dt <- filter_events_before_start.dt(dt)
    if (!purrr::is_empty(end_events)) {
      dt <- filter_events_after_end.dt(dt)
    }
  }
  dt <- filter_tailing_events.dt(dt)
  dt[, mask := NULL]

  # VALIDATION STEP
  dups <- dt[, .(d = any(duplicated(timestamp))), by = context]
  stopifnot(!any(dups$d))
  # .duplicates <- dt |>
  #  summarise(
  #    n = n(),
  #    events = paste0(event, collapse = ","),
  #    .by = all_of(c("timestamp", context))
  #  ) |>
  #  filter(n > 1) |>
  #  select(!n)
  # stopifnot(rlang::is_empty(.duplicates))

  dt[, offset := timestamp - min(timestamp), by = c("run", context)]


  return(dt)
}
