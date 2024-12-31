method_fit <- function(df.trans, fit_fun, by = NULL) {
  by <- unique(c("event.from", "event.to", by))

  fitted <- df.trans |>
    group_by(across(all_of(by))) |>
    summarise(
      max_duration = max(duration),
      min_duration = min(duration),
      count = n(),
      params = list(fit_fun(duration)),
      .groups = "keep"
    )

  Q <- fitted |>
    dplyr::select(all_of(c(by, "params")))

  return(Q)
}

method_fit.matrix <- function(..., .simplification = F) {
  Q <- method_fit(...)

  Q <- Q |>
    tidyr::unnest_longer(params) |>
    # dyplr::mutate(params = list(params)) |>
    reshape2::acast(event.from ~ event.to ~ params_id, drop = FALSE, value.var = "params")

  if (.simplification) {
    row_has_data <- apply(Q, 1, \(x) any(!is.na(x)))
    col_has_data <- apply(Q, 2, \(x) any(!is.na(x)))
    Q.simplified <- Q[row_has_data, col_has_data, , drop = FALSE]
    Q <- Q.simplified
  }

  return(Q)
}
