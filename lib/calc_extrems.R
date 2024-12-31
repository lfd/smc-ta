calc_extrems <- function(df, by) {
  by_extrems <- df |>
    dplyr::summarise(
    	max = max(duration),
    	mean = mean(duration),
    	median = median(duration),
    	sd = sd(duration),
    	min = min(duration),
      .by = all_of(by)
    ) |>
  	tidyr::pivot_longer(!by)

  return(by_extrems)
}
