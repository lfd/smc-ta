vis_compare_trans <- function(df, save_func = function(...) {}) {
  save <- function(plot_name, ...) {
    save_func(paste0("compare-trans-", plot_name))
  }

  required_columns <- c("event.from", "event.to", "duration", "type")
  stopifnot(all(required_columns %in% colnames(df)))

  df <- df |>
    dplyr::filter(!is.na(event.to))|>
    dplyr::mutate(
      max = max(duration),
      q99 = quantile(probs = c(.99), duration) |> unlist(),
      q95 = quantile(probs = c(.95), duration) |> unlist(),
      q90 = quantile(probs = c(.90), duration) |> unlist(),
      .by = c("type", "transition")
    )

  plots <- list()

  plot_name <- "duration-histline"
  plots[[plot_name]] <- .vis_compare_trans_histline(df)
  save(plot_name)

  plot_name <- "duration-histline-log"
  plots[[plot_name]] <- .vis_compare_trans_histline_log(df)
  save(plot_name)

  plot_name <- "duration-histline-annotated"
  plots[[plot_name]] <- .vis_compare_trans_histline_annotated(df)
  save(plot_name)

  plot_name <- "successor-prop"
  plots[[plot_name]] <- df |>
    ggplot(aes(x = event.to, fill = type)) +
  	# in case there is only one type, stat_count can be used instead of stat_prop.
    stat_prop(aes(y = after_stat(prop), by = type), position = "dodge2") +
    scale_y_continuous(label = scales::label_percent(suffix = "\\%")) +
    facet_wrap(~event.from, ncol = 2) +
    labs(title = "Successor state", y = "Probability", x = "Successor", fill = "")
  save(plot_name)

  plot_name <- "predecessor-prop"
  plots[[plot_name]] <- df |>
    ggplot(aes(x = event.from, fill = type)) +
  	# in case there is only one type, stat_count can be used instead of stat_prop.
    stat_prop(aes(y = after_stat(prop), by = type), position = "dodge2") +
    scale_y_continuous(label = scales::label_percent(suffix = "\\%")) +
    facet_wrap(~event.to, ncol = 2) +
    labs(title = "Predecessor state", y = "Probability", x = "Predecessor", fill = "")
  save(plot_name)

  return(plots)
}

.vis_compare_trans_histline <- function(df) {
  plot <- df |>
    ggplot(aes(x = duration, color = type)) +
    # stat_bin(aes(y = after_stat(ndensity)), position = "jitter", bins = 200, fill = NA, alpha = 0) +
    stat_bin(aes(y = after_stat(density)), position = "identity", geom = "line", bins = 250) +
    scale_x_continuous(label = scales::label_number(scale_cut = scales::cut_si("s"))) +
    labs(x = "Duration", y = "Density", color = "Data Type") +
    facet_wrap(~transition, scales = "free", ncol = 2)

  return(plot)
}

.vis_compare_trans_histline_annotated <- function(df) {
	extr_values <- df |>
		dplyr::select(max, q99, q95, q90, type, transition) |>
		unique() |>
		pivot_longer(cols = !c(type, transition))

  plot <- df |>
    .vis_compare_trans_histline() +
    geom_vline(data = extr_values, aes(xintercept = value, color = type, linetype = name), show.legend = F) +
    geom_text(data = extr_values, aes(x = value, y = 1, label = scales::label_number(scale_cut = scales::cut_si("s"))(value)), show.legend = F, check_overlap = TRUE)

  return(plot)
}

.vis_compare_trans_histline_log <- function(df) {
  plot <- df |>
    .vis_compare_trans_histline() +
    scale_x_log10(label = scales::label_number(scale_cut = scales::cut_si("s")))

  return(plot)
}
