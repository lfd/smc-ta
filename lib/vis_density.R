vis_density <- function(df, by) {
	df |>
    ggplot(aes(x = duration, color = .data[[by]], y = after_stat(ndensity))) +
    #stat_bin(bins = 100) +
    stat_density(geom = "line") +
    scale_x_continuous("Duration", expand = c(0,0), labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    scale_y_continuous("nDensity [a.u.]", expand = c(0,0), breaks = c()) +
    facet_wrap(
      c(by),
      scales = "free_x",
      shrink = TRUE
    )
}
