#' Process and Transform Event Log Data
#'
#' 1. Ensure monotonic time.
#' 2. Add column 'run' grouping events to an execution pass.
#' 3. Add column 'offset' indicating the time elapsed since the staring event.
#' 4. Remove self-loops by adding counter to event
#'
#' @param df A data frame containing event data with at least "timestamp" and "event" columns.
#' @param start_events A character vector of event names that indicate the start of a run.
#' @param end_events A character vector of event names that indicate the end of a run.
#' @return A modified data frame with additional columns for run identification and modified event labels.
#'
#' @export
prepare_event_log <- function(df, start_events, end_events, context) {
  by <- c("run", {{ context }})

  # cluster <- new_cluster(2)

  df <- prepare_event_log_fast(df, start_events, end_events, context)
  rlang::inform("Finished fast data preparation")
  df <- df %>%
    mutate(
      nr = row_number(),
      dup = coalesce(lag(event), "") == event,
      r = if_else(dup, NA, nr),
      # r = if_else(coalesce(lag(event), "") == event, NA, row_number())
      .by = all_of(by)
    ) |>
    select(!dup) |>
    # if we use data.tables
    # setnafill("locf", cols=c("r")) |>
    tidyr::fill(r, .direction = "down") |>
    mutate(
      event_c = nr - r,
      event = if_else(event_c > 0, paste0(event, event_c), event) |> factor(),
      .by = all_of(by),
      .keep = "all"
    )

  return(df)
}

prepare_event_log.dt <- function(dt, start_events, end_events, context) {
  if (!is.data.table(dt)) {
    stop("Input must be a data.table")
  }

  by <- c("run", context)

  dt <- prepare_event_log_fast.dt(dt, start_events, end_events, context)
  setDT(dt)
  rlang::inform(
    "Finished fast data preparation",
    body = summarise(dt, .by = "event", count = n()) |> format()
  )
  dt[, nr := .I, by = by]
  dt[, dup := shift(event, type = "lag") == event, by = by]
  dt[, dup := fcoalesce(dup, FALSE)]
  dt[, r := fifelse(dup, NA_integer_, nr), by = by]
  dt[, r := nafill(r, type = "locf"), by = by]
  dt[, event_c := nr - r, by = by]

  ## This takes too long
  # dt[, event := factor(fifelse(event_c > 0, paste0(event, event_c), as.character(event))), by = by]

  ## so we do
  .ddt <- dt[event_c > 0]
  additional_level <- unique(paste0(.ddt$event, .ddt$event_c))
  new_levels <- c(levels(dt$event), additional_level)
  setattr(dt$event, "levels", new_levels)
  dt[, event := fifelse(event_c > 0, factor(paste0(event, event_c), levels = new_levels), event), by = by]

  stopifnot(nrow(dt[is.na(event)]) == 0)

  # Cleanup added columns
  dt[, c("nr", "dup", "r", "event_c") := NULL]

  return(dt)
}

#' Process and Transform Event Log Data
#'
#' 1. Ensure monotonic time.
#' 2. Add column 'run' grouping events to an execution pass.
#' 3. Add column 'offset' indicating the time elapsed since the staring event.
#'
#' @param df A data frame containing event data with at least "timestamp" and "event" columns.
#' @param start_events A character vector of event names that indicate the start of a run.
#' @param end_events A character vector of event names that indicate the end of a run.
#' @return A modified data frame with additional columns for run identification.
#'
#' @export
prepare_event_log_fast <- function(df, start_events, end_events, context) {
  required_columns <- c("timestamp", "event")
  stopifnot(all(required_columns %in% colnames(df)))

  filter_events_after_end <- function(x) {
    xx <- filter(x, run == mask)
    count <- nrow(x) - nrow(xx)
    if (count > 0) {
      rlang::inform(R.utils::printf("Skip %d events occuring after runs's end event", count))
    }
    return(dplyr::select(xx, !mask))
  }

  filter_events_before_start <- function(x) {
    xx <- filter(x, run != 0)
    count <- nrow(x) - nrow(xx)
    if (count > 0) {
      rlang::inform(R.utils::printf("Skip %d events occuring before any start event", count))
    }
    return(xx)
  }

  # Stop if there are events within the same context with duplicate timestamps
  df |>
    summarise(
      duplicated = any(duplicated(timestamp)),
      .by = all_of({{ context }})
    ) |>
    summarise(!any(duplicated)) |>
    unlist() |>
    stopifnot()

  tidy_df <- df |>
    arrange(timestamp) |>
    mutate(
      run = cumsum(dplyr::if_else(event %in% start_events, 1, 0)) |> as.integer(),
      mask = 1 + cumsum(dplyr::if_else(lag(event) %in% end_events, 1, 0)),
      .by = all_of({{ context }})
    ) |>
    filter_events_before_start() |>
    filter_events_after_end() |>
    mutate(
      offset = timestamp - min(timestamp),
      .by = all_of(c("run", {{ context }}))
    )

  return(tidy_df)
}

prepare_event_log_fast.dt <- function(dt, start_events, end_events, context, with.filtering = T) {
  if (!is.data.table(dt)) {
    stop("Input must be a data.table")
  }

  required_columns <- c("timestamp", "event")
  if (!all(required_columns %in% colnames(dt))) {
    stop("Following columns are required: ", paste(required_columns, collapse = ", "))
  }

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

