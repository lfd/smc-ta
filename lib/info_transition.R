#' Generate information about a transition data frame
#'
#' This function takes a transition data frame with columns `event.from`,
#' `event.to`, and `duration`, and returns a list containing various statistics
#' related to the transitions between events.
#'
#' @param transition_data A data frame with columns `event.from`, `event.to`,
#'   and `duration`. The `event.from` column should contain a categorical
#'   variable representing the type of event that occurred previously. The
#'   `event.to` column should contain a categorical variable representing the
#'   type of event that occurred next. The `duration` column should contain the
#'   duration between the previous and next events, in seconds or some other
#'   unit of time.
#' @return A list containing statistics related to the transitions between
#'   events in the input transition data frame. The list contains the following
#'   elements:
#'   - `names`: A character vector containing the names of all unique transitions between event types in the transition data frame.
#'   - `data`: A data frame with columns `transition`, `event.from`, `event.to`, `count`, `mean`, `median`, `min`, and `max`. The `transition` column contains the name of the transition between event types. The `event.from` and `event.to` columns contain the names of the previous and next event types, respectively. The `count` column contains the number of times each transition occurred in the input data frame. The `mean`, `median`, `min`, and `max` columns contain the mean, median, minimum, and maximum duration for each transition, respectively.
#'
#' @export
info_transition <- function(df) {
  by = c("event.from", "event.to")
  required_columns <- c("duration", by)
  stopifnot(all(required_columns %in% colnames(df)))

  duration_stat <- df |>
    summarize(
      count = n(),
      mean = mean(duration),
      median = median(duration),
      min = min(duration),
      max = max(duration),
      .by = all_of(by)
    ) |>
    mutate(
      loop = event.from == event.to
    )

  names <- paste_trans(df$event.from, df$event.to) |> unique()

  return(list(
    names = names,
    data = duration_stat
  ))
}
