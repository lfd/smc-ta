vis_ecdf <- function(df, by, comparison, with_quantiles = T, with_max = F) {
  plot <- df |>
    ggplot(aes(x = duration, y = 1 - after_stat(ecdf), color = factor(.data[[comparison]]))) +
    stat_ecdf() +
    scale_x_continuous("Duration", limits = c(0, NA), labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    labs(
      y = "1-eCDF",
      color = str_to_title(comparison)
    ) +
    theme_paper_base() +
    theme(legend.box = "vertical")

  if (str_equal(comparison, "method")) {
    plot <- plot +
      labs(color = NULL)
  }

  if (with_quantiles) {
    .group <- discard(c(by, comparison), is.null)

    by_quantiles <- calc_quantiles(df, .group)
    by_extrems <- calc_extrems(df, .group)

    if (!is.null(by) && str_equal(by, "path")) {
      by_quantiles <- by_quantiles |> rbind(calc_quantiles(df, c(comparison)) |> mutate(path = "all"))
      by_extrems <- by_extrems |> rbind(calc_extrems(df, c(comparison)) |> mutate(path = "all"))
    }

    plot <- plot +
      geom_vline(
        data = by_quantiles,
        aes(
          xintercept = value,
          color = factor(.data[[comparison]]),
          linetype = as_factor(name * 100) |> forcats::fct_relabel(~ paste0(.x, "%"))
        )
      ) +
      labs(linetype = "Quantile")
    if (with_max) {
      plot <- plot +
        geom_vline(
          data = filter(by_extrems, name == "max"),
          aes(
            xintercept = value,
            color = factor(.data[[comparison]]),
            linetype = as_factor(100) |> forcats::fct_relabel(~ paste0(.x, "%"))
          ),
          linewidth = 1,
          show.legend = FALSE
        ) +
        scale_linewidth(guide = "none")
    }
  } else {
    .x_max <- max(calc_quantiles(df, c(comparison), probs = 0.99)$value)
    plot <- plot +
      coord_cartesian(xlim = c(0, .x_max))
  }

  if (!is.null(by) && str_equal(by, "path")) {
    plot <- plot +
      stat_ecdf(data = mutate(df, path = "all"), aes(x = duration, y = 1 - after_stat(ecdf), color = factor(.data[[comparison]])))
  }

  if (!is.null(by)) {
    plot <- plot +
      # facet_grid(rows = vars(.data[[by]]), scales = "free_x")
      facet_wrap(vars(.data[[by]]), scales = "free")
  }

  return(plot)
}
