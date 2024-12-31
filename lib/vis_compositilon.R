vis_composition <- function(df, composition, by = NULL) {
  group <- discard(c(by, composition), is.null)
  by_quantiles <- calc_quantiles(df, group)
  by_extrems <- calc_extrems(df, group)

  if (composition == "path" && is.null(by)) {
    by_quantiles <- by_quantiles |> rbind(calc_quantiles(df, NULL) |> mutate(path = "all"))
    by_extrems <- by_extrems |> rbind(calc_extrems(df, NULL) |> mutate(path = "all"))
  }

  plot <- df |>
    ggplot2::ggplot(aes(x = duration)) +
    stat_bin(aes(fill = .data[[composition]]), binwidth = 10e-5, position = "fill", alpha = 0.7) +
    stat_density(aes(y = after_stat(ndensity), color = .data[[composition]]), linewidth = 1, position = "identity", geom = "line") +
    geom_vline(
      data = by_quantiles,
      aes(xintercept = value, color = .data[[composition]], linetype = factor(name))
    ) +
    geom_vline(
      data = filter(by_extrems, name == "max"),
      aes(xintercept = value, color = .data[[composition]], linetype = "1"),
      linewidth = 1,
      show.legend = FALSE
    ) +
    scale_x_continuous(
      labels = scales::label_number(scale_cut = scales::cut_si("s")),
      limits = c(0, NA),
      expand = expand_scale(mult = c(0, 0.1))
    ) +
    scale_fill_discrete(str_to_title(composition)) +
    scale_color_discrete(str_to_title(composition)) +
    scale_linetype_discrete("Quantile") +
    scale_y_continuous("Normalized Density", expand = c(0, 0), breaks = c()) +
    labs(x = "Duration") +
    theme_paper_base()

  if (!is.null(by)) {
    plot <- plot +
      facet_wrap(vars(.data[[by]]), scales = "free")
  } else if (composition == "path") {
    plot <- plot +
      stat_density(data = df, aes(y = after_stat(ndensity), color = "all"), linewidth = 1, position = "identity", geom = "line")
  }

  return(plot)
}
