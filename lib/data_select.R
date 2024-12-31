data_select <- function(df.trans, n, by = c("run"), randomized = FALSE) {
  by <- unique(c(by, "run")) # Ensure run is present

  if (n == 1 && !randomized) {
    return(df.trans)
  }

  groups <- distinct(df.trans, across(all_of(by)))

  if (n <= 1) {
    n <- floor(nrow(groups) / n)
  }

  if (n > nrow(groups)) {
    warning(sprintf("Section exceeds base data: cannot select %d rows; data only contains %d", n, nrow(groups)))
  }

  if (randomized) {
    selection <- slice_sample
  } else {
    selection <- slice_head
  }

  groups |>
    selection(n = n) |>
    inner_join(df.trans, by = all_of(by))
}
