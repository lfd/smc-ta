#' Plot data as time series with 3 kind of quantiles facet wrapped by "by"
#' 1. quantile over all data
#' 2. normal quantile regression
#' 3. additive quantile regression smoothing (AQRS)
vis_time_series <- function(df, by, with_quantiles = T, with_AQRS = F) {
  probs <- 1 - 10**-seq(2, 4)
  by_quantiles <- calc_quantiles(df, by)

  plot <- df |>
    ggplot2::ggplot(aes(x = run, y = duration)) +
    geom_point() +
    geom_quantile(
      aes(color = "Quantile Regression", linetype = factor(after_stat(quantile))),
      quantiles = probs,
    )
  if (with_AQRS) {
    plot <- plot + geom_quantile(
      aes(color = "AQRS", linetype = factor(after_stat(quantile))),
      quantiles = probs,
      method = "rqss", # Using Additive Quantile Regression Smoothing (AQRS)
      lambda = .5 # prevent overfitting by imposing penalties on the size of the coefficients
    )
  }
  if (with_quantiles) {
    plot <- plot +
      geom_hline(
        data = by_quantiles,
        aes(yintercept = value, color = "Quantile", linetype = factor(name))
      )
  }
  plot <- plot +
    scale_x_discrete(labels = scales::label_number()) +
    scale_y_continuous("Duration", expand = c(0, 0), labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    labs(x = "Iteration", linetype = "Quantile", color = NULL) +
    facet_wrap(vars(.data[[by]]), scales = "fixed") +
    theme_paper_base()

  return(plot)
}
