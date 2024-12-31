vis_transition <- function(df, save_func = function(...) {}, data_type = NULL) {
  save <- function(plot_name, ...) {
    save_func(paste0(data_type, "-transition-", plot_name))
  }

  # Tidy data
  vis <- df |>
    # dplyr::filter(!is.na(duration)) |> # This filter should not be necessary
    dplyr::filter(!is.na(event.to)) |>
    # Ensure transition names are factor.
    dplyr::mutate(transition = as.factor(transition))

  # clip plots at quantiles (TODO)
  .q <- 0.999
  qu <- vis |>
    dplyr::summarise(q = quantile(duration, .q), .by = c(transition)) |>
    dplyr::select(q) |>
    max()

  ql <- vis |>
    dplyr::summarise(q = quantile(duration, 1 - .q), .by = c(transition)) |>
    dplyr::select(q) |>
    min()


  plots <- list()

  plot_name <- "duration-qq"
  plots[[plot_name]] <- vis |>
    ggplot(aes(sample = duration, color = transition)) +
    stat_qq() +
    stat_qq_line() +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    facet_wrap(~transition) +
    labs(title = "Q-Q Plot", color = "Transition") +
    guides(color = "none") +
    theme(legend.position = "right")
  save(plot_name)

  plot_name <- "duration-boxplot"
  plots[[plot_name]] <- vis |>
    ggplot(aes(x = transition, y = duration)) +
    stat_boxplot() +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    labs(x = "Transition", y = "Duration")
  save(plot_name)

  plot_name <- "count-tile"
  plots[[plot_name]] <- df |>
    ggplot(aes(y = event.from, x = event.to)) +
    stat_bin2d(drop = FALSE, na.rm = TRUE, geom = "tile", color = "black") +
    stat_bin2d(aes(label = after_stat(count)), drop = FALSE, geom = "text", show.legend = F) +

    # stat_bin2d(aes(label = after_stat(count), color = after_stat(count)), drop = FALSE, geom = "text", show.legend = F) +
    # scale_color_gradient(NULL, low = "black", high = "grey100") +

    scale_x_discrete("State To") +
    scale_y_discrete("State From") +
    scale_fill_gradient("Count", low = "grey90", high = "red") +
    theme(
      legend.position = "right",
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank()
    )
  save(plot_name)


  plot_name <- "successor-prop"
  plots[[plot_name]] <- vis |>
    ggplot(aes(x = event.to)) +
    stat_count() +
    facet_wrap(~event.from, ncol = 2, scales = "free_y") +
    labs(title = "Successor state", y = "Probability", x = "Successor")
  save(plot_name)

  plot_name <- "predecessor-prop"
  plots[[plot_name]] <- vis |>
    ggplot(aes(x = event.from)) +
    stat_count() +
    facet_wrap(~event.to, ncol = 2, scales = "free_y") +
    labs(title = "Predecessor state", y = "Probability", x = "Predecessor")
  save(plot_name)


  plot_name <- "duration-distribution"
  plots[[plot_name]] <- vis |>
    ggplot(aes(x = duration, fill = transition, y = after_stat(ndensity))) +
    stat_bin(bins = 100) +
    stat_density(color = "black", alpha = 0) +
    geom_label(
      data = summarise(vis, .by = c("transition"), total_count = n()),
      aes(x = Inf, y = 1, label = paste("Count: ", total_count)),
      hjust = 1.1, vjust = 1.1, size = 5
    ) +
    scale_x_continuous("Duration", labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    scale_y_continuous("nDensity [a.u.]") +
    facet_wrap(
      c("transition"),
      scales = "free",
      shrink = TRUE,
      nrow = 3
    ) +
    guides(fill = "none")
  save(plot_name)

  plot_name <- "duration-distribution-log10"
  plots[[plot_name]] <- plots[["duration-distribution"]] +
    scale_x_log10("Duration", labels = scales::label_number(scale_cut = scales::cut_si("s")))
  save(plot_name)

  return(plots)
}
