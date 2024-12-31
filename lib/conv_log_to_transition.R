#' Convert Event Log to Transitions
#'
#' This function takes an event log data frame and converts it into a transition data frame
#' by calculating various attributes such as event transitions, time since the start, and
#' duration of transitions.
#'
#' @param df df A data frame with columns `timestamp`, `event`, `offset`, and `run`. The `timestamp` column should contain the time of each event in seconds since some start time. The `event` column should contain a categorical variable representing the type of event. The `offset` column should contain the offset of each event from the start time, in seconds. The `run` column should contain a categorical variable representing the run or session to which each event belongs.
#' @return A data frame with columns `run`, `since.start`, `duration`, `event.from`, and `event.to`. Each row represents a transition from one event to another, with the start time, duration, and category of both events.
#'
#' @importFrom dplyr mutate lead arrange
#'
#' @export
conv_log_to_transition <- function(df, context=NULL) {
  by <- c("run", context)
  required_columns <- c("timestamp", "event", "offset", by)
  stopifnot(all(required_columns %in% colnames(df)))

  transitions <- df |>
    # Ensure that the `event` column is a factor
    dplyr::mutate(event = as.factor(event)) |>
    dplyr::mutate(
      event.from = event,
      event.to = lead(event),
      since.start = offset,
      duration = lead(timestamp) - timestamp,
      .by = all_of(by),
      # .keep = "none"
    ) #|>
  # dplyr::arrange(run, since.start)

  return(transitions)
}

conv_log_to_transition.dt <- function(dt, context=NULL) {
  by <- c("run", context)
  required_columns <- c("timestamp", "event", "offset", by)
  stopifnot(all(required_columns %in% colnames(dt)))

  transitions <- dt
  transitions[, `:=`(event.to = shift(event, type = "lead"), duration = shift(timestamp, type = "lead") - timestamp), by = by]
  setnames(transitions, c("event","offset"), c("event.from", "start.since"))

  return(transitions)
}
