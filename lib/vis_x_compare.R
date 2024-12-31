vis_x_compare <- function(df, by, plot_type, comparison, with_max = F) {
  df <- df |>
    filter(!is.na(duration))

  if (is.null(by)) {
    by <- "group"
    df <- df |>
      add_column(group = "total")
  }

  plot <- NULL

  by_quantiles <- calc_quantiles(df, c(by, comparison))
  by_extrems <- calc_extrems(df, c(by, comparison))

  if (by == "path") {
    by_quantiles <- by_quantiles |> rbind(calc_quantiles(df, c(comparison)) |> mutate(path = "all"))
    by_extrems <- by_extrems |> rbind(calc_extrems(df, c(comparison)) |> mutate(path = "all"))
  }


  if (plot_type == "ridges") {
    plot <- df |>
      ggplot(aes(
        x = duration,
        y = factor(.data[[comparison]]),
        # color = factor(.data[[comparison]])
      )) +
      stat_density_ridges(alpha = 0) +
      geom_segment(
        data = by_quantiles,
        aes(
          linetype = paste0(name * 100, "%"),
          color = paste0(name * 100, "%"),
          x = value,
          xend = value,
          y = as.numeric(factor(.data[[comparison]])),
          yend = as.numeric(factor(.data[[comparison]])) + .9,
          # color = factor(.data[[comparison]])
        ),
      ) +
      geom_segment(
        data = filter(by_extrems, name == "max"),
        aes(
          linetype = paste0(1 * 100, "%"),
          color = paste0(1 * 100, "%"),
          x = value,
          xend = value,
          y = as.numeric(factor(.data[[comparison]])),
          yend = as.numeric(factor(.data[[comparison]])) + .9,
          # color = factor(.data[[comparison]])
        ),
        linewidth = 1,
        show.legend = F # hide linewidth dimension in legend
      ) +
      labs(
        linetype = "Quantile",
        y = str_to_title(comparison),
        color = "Quantile"
      ) +
      theme_paper_ridges()
    if (by == "path") {
      plot <- plot +
        stat_density_ridges(data = mutate(df, path = "all"), aes(
          x = duration, y = factor(.data[[comparison]]),
          # color = factor(.data[[comparison]])
        ), alpha = 0)
    }
    if (str_equal(comparison, "method")) {
      plot <- plot +
        labs(y = NULL)
    }
  } else {
    plot <- df |>
      ggplot(aes(x = duration, color = factor(.data[[comparison]])))
    if (plot_type == "density") {
      plot <- plot +
        stat_density(aes(y = after_stat(ndensity)), geom = "line", position = "identity")
      if (by == "path") {
        plot <- plot +
          stat_density(data = mutate(df, path = "all"), aes(y = after_stat(ndensity)), geom = "line", position = "identity")
      }
    } else if (plot_type == "histogram" || plot_type == "frequency") {
      plot <- plot +
        geom_freqpoly(aes(y = after_stat(density)), binwidth = 10e-5, position = "identity")
      if (by == "path") {
        plot <- plot +
          geom_freqpoly(data = mutate(df, path = "all"), aes(y = after_stat(density)), binwidth = 10e-5, position = "identity")
      }
    }
    plot <- plot +
      geom_vline(
        data = by_quantiles,
        aes(xintercept = value, color = factor(.data[[comparison]]), linetype = paste0(name * 100, "%"))
      ) +
      scale_linetype("Quantile") +
      scale_y_continuous("Density", breaks = NULL) +
      labs(color = str_to_title(comparison)) +
      theme_paper_base() +
      theme(legend.box = "vertical")
    if (str_equal(comparison, "method")) {
      plot <- plot +
        labs(color = NULL)
    }
    if (with_max) {
      plot <- plot +
        geom_vline(
          data = filter(by_extrems, name == "max"),
          aes(xintercept = value, color = factor(.data[[comparison]]), linetype = paste0(1 * 100, "%")),
          linewidth = 1,
          show.legend = F # hide linewidth dimension in legend
        )
    }
  }

  plot <- plot +
    scale_x_continuous("Duration",
      expand = c(0, 0),
      limits = c(0, NA),
      label = scales::label_number(scale_cut = scales::cut_si("s"))
    )

  if (!str_equal(by, "group")) {
    plot <- plot +
      facet_grid(rows = vars(.data[[by]]), scales = "free")
  }

  return(plot)
}
