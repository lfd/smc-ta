method_fit <- function(df, fit_fun, by = NULL) {
  library(dplyr)

  by <- c("event.from", "event.to", by) |> unique()
  required_columns <- c("duration", by)
  stopifnot(all(required_columns %in% colnames(df)))

  fitted <- df |>
    group_by(across(all_of(by))) |>
    summarise(
      max_duration = max(duration),
      min_duration = min(duration),
      count = n(),
      params = list(fit_fun(duration)),
      .groups = "keep"
    )

  Q <- fitted |>
    select(all_of(c(by, "params")))

  return(Q)
}

method_fit.matrix <- function(..., .simplification = F) {
  Q <- method_fit(...) |>
    filter(!is.na(event.to)) |>
    reshape2::acast(event.from ~ event.to, drop = FALSE, value.var = "params")

  if (.simplification) {
    row_has_data <- apply(Q, 1, \(x) any(!is.na(x)))
    col_has_data <- apply(Q, 2, \(x) any(!is.na(x)))
    Q.simplified <- Q[row_has_data, col_has_data, , drop = FALSE]
    Q <- Q.simplified
  }

  return(Q)
}
