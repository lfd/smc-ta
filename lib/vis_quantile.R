#' @param df A dataframe
#' @param by facet var
#' @param comparison color var
#' @param plot_type A character vector with possible values: "plain", "diff", "diff-abs", or "diff-rel"
vis_quantile <- function(df, by = NULL, comparison, plot_type = "plain", many_quantiles = F) {
  probs <- NULL # define variable in this scope
  if (many_quantiles) {
    probs <- c(seq(from = 0.75, to = 0.90, by = 0.05), seq(0.95, 0.98, by = 0.01), (1 - 0.1**seq(2, 5)))
  } else {
    probs <- c((1 - 0.1**seq(1, 5)))
  }

  group <- unique(discard(c(by, comparison, "method"), is.null))
  by_quantiles <- calc_quantiles(df, group, probs)
  # by_extrems <- calc_extrems(df, c(by, comparison))

  if (!is.null(by) && str_equal(by, "path")) {
    by_quantiles <- by_quantiles |> rbind(calc_quantiles(df, discard(group, \(x) str_equal(x, by)), probs) |> mutate(path = "all"))
    # by_extrems <- by_extrems |> rbind(calc_extrems(df, c(comparison)) |> mutate(path = "all"))
  }

  plot <- NULL # define variable in this scope

  if (str_starts(plot_type, "diff")) {
    x <- filter(by_quantiles, method != "measurement")
    y <- filter(by_quantiles, method == "measurement")
    by_quantiles <- full_join(x, y, by = discard(c(by, "name"), is.null), keep = NULL) |>
      mutate(
        value = value.x - value.y,
        sign = sign(value),
        abs = abs(value),
        method = method.x,
        !!comparison := .data[[paste0(comparison, ".x")]]
        # .keep = "unused"
      )
  }
  if (str_equal(plot_type, "diff-abs")) {
    by_quantiles <- by_quantiles |>
      mutate(value = abs)
  } else if (str_equal(plot_type, "diff-rel")) {
    by_quantiles <- by_quantiles |>
      mutate(value = (value.x / value.y) - 1)
  }

  plot <- by_quantiles |>
    # mutate(name = factor(scales::label_percent()(name))) |>
    ggplot(aes(x = factor(name), y = value, color = factor(.data[[comparison]]))) +
    geom_line(aes(group = factor(.data[[comparison]]))) +
    # geom_point(aes(shape = factor(sign)), size = 2) +
    geom_point(size = 2) +
    # scale_x_continuous("Quantile", labels = scales::label_percent()) +
    scale_x_discrete("Quantile", labels = sapply(probs, function(.x) paste0(.x * 100, "%"))) +
    labs(color = str_to_title(comparison)) +
    theme_paper_base()

  if (str_equal(comparison, "method")) {
    plot <- plot +
      labs(color = NULL)
  }

  if (!is.null(by)) {
    plot <- plot +
      facet_grid(rows = vars(.data[[by]]), scales = "free")
  }

  if (str_equal(plot_type, "diff-abs")) {
    plot <- plot +
      scale_y_sqrt("Duration abs. Difference", limits = c(0, NA), labels = scales::label_number(scale_cut = scales::cut_si("s")))
  } else if (str_equal(plot_type, "diff-rel")) {
    plot <- plot +
      scale_y_continuous("Duration rel. Difference", labels = scales::label_percent())
  } else if (str_equal(plot_type, "diff")) {
    plot <- plot +
      # scale_y_sqrt("Duration Difference", labels = scales::label_number(scale_cut = scales::cut_si("s")))
      scale_y_continuous("Duration Difference", labels = scales::label_number(scale_cut = scales::cut_si("s")))
  } else # plot_type == "plain"
  {
    plot <- plot +
      scale_y_continuous("Duration", labels = scales::label_number(scale_cut = scales::cut_si("s")))
  }

  return(plot)
}

