#' Generate information about a event log data frame
#'
#' This function takes a log data frame with columns `event` and `run`, and
#' returns a list containing various statistics and counts related to the events
#' and runs.
#'
#' @param log_data A data frame with columns `event` and `run`. The `event`
#'   column should contain a categorical variable representing the type of event.
#'   The `run` column should contain a categorical variable representing the run or
#'   session to which each event belongs.
#' @return A list containing statistics and counts related to the events and
#'   runs in the input log data frame. The list contains the following elements:
#'   - `observation_count`: The total number of events in the log data frame.
#'   - `observation_counts`: A named integer vector containing the number of occurrences of each event type in the log data frame.
#'   - `event_type_counts`: The number of unique event types in the log data frame.
#'   - `event_names`: A character vector containing the names of all event types in the log data frame.
#'   - `run_count`: The total number of runs or sessions in the log data frame.
#'   - `starting_event_names`: A character vector containing the name of the first event type in each run or session.
#'   - `starting_event_counts`: A named integer vector containing the number of times each event type appears as the first event in a run or session.
#'   - `ending_event_names`: A character vector containing the name of the last event type in each run or session.
#'   - `ending_event_counts`: A named integer vector containing the number of times each event type appears as the last event in a run or session.
#'
#' @importFrom dplyr group_by summarise n first last count
#' @importFrom purrr set_names
#'
#' @export
info_log <- function(df) {
  required_columns <- c("event", "run")
  stopifnot(all(required_columns %in% colnames(df)))

  counts <- summarise(df, .by = "event", count = n())
  first_last_event <- summarise(df, .by = "run", start_event = first(event), end_event = last(event))
  start_counts <- count(first_last_event, start_event)
  end_counts <- count(first_last_event, end_event)

  list(
    observation_count = nrow(df),
    observation_counts = magrittr::set_names(counts$count, counts$event),
    event_type_counts = n_distinct(df$event),
    event_names = unique(df$event),
    run_count = n_distinct(df$run),
    starting_event_names = unique(first_last_event$start_event),
    starting_event_counts = magrittr::set_names(start_counts$n, start_counts$start_event),
    ending_event_names = unique(first_last_event$end_event),
    ending_event_counts = magrittr::set_names(end_counts$n, end_counts$end_event)
  )
}
