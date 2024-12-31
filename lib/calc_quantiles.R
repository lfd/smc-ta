calc_quantiles <- function(df, by, probs = c(.99, .999, .9999)) {
  by_quantiles <- df |>
    dplyr::summarise(
      value = list(quantile(probs = probs, duration, names = F)),
      .by = all_of(by)
    ) |>
    tidyr::unnest_longer(value) |>
    dplyr::mutate(name = probs, .by = all_of(by))

  return(by_quantiles)
}
