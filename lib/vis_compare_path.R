vis_compare_path <- function(df, save_func = function(...) {}) {
  save <- function(plot_name, ...) {
    save_func(paste0("compare-path-", plot_name))
  }

  required_columns <- c("path", "type", "duration")
  stopifnot(all(required_columns %in% colnames(df)))

  df <- df |>
    # mutate(q = list(quantile(probs = c(.8, .9), duration))) |> unnest_wider(q)
    dplyr::mutate(
      max = max(duration),
      q99 = quantile(probs = c(.99), duration) |> unlist(),
      q95 = quantile(probs = c(.95), duration) |> unlist(),
      q90 = quantile(probs = c(.90), duration) |> unlist(),
      .by = c(type)
    )

  plots <- list()

  plot_name <- "cum-duration-histline"
  plots[[plot_name]] <- .vis_compare_path_histline(df)
  save(plot_name)


  plot_name <- "cum-duration-histline-detail"
  plots[[plot_name]] <- .vis_compare_path_histline_detail(df)
  save(plot_name)


  plot_name <- "cum-duration-histline-annotated"
  plots[[plot_name]] <- .vis_compare_path_histline_annotated(df)
  save(plot_name)

  plot_name <- "cum-duration-histline-log"
  plots[[plot_name]] <- .vis_compare_path_histline_log(df)
  save(plot_name)

  plot_name <- "cum-duration-histline-log-annotated"
  plots[[plot_name]] <- .vis_compare_path_histline_log_annotated(df)
  save(plot_name)

  plot_name <- "cum-max-duration-histline"
  plots[[plot_name]] <- .vis_compare_path_histline_q99(df)
  save(plot_name)

  return(plots)
}

.vis_compare_path_histline <- function(df) {
  plot <- df |>
    ggplot(aes(x = duration, color = type)) +
    # stat_bin(aes(y = after_stat(ndensity)), position = "jitter", bins = 200, fill = NA, alpha = 0) +
    stat_bin(aes(y = after_stat(density), color = NULL, linetype = type), position = "identity", geom = "line", bins = 250) +
    scale_x_continuous(label = scales::label_number(scale_cut = scales::cut_si("s"))) +
    labs(x = "Duration", y = "Density", color = "Data Type")

  return(plot)
}

.vis_compare_path_histline_detail <- function(df) {
  plot <- df |>
    ggplot(aes(x = duration, color = path)) +
    # stat_bin(aes(y = after_stat(ndensity)), position = "jitter", bins = 200, fill = NA, alpha = 0) +
    stat_bin(aes(y = after_stat(density)/3, color = NULL, fill = path), position = "stack", bins = 250) +
    stat_bin(aes(y = after_stat(density)), position = "identity", geom = "line", bins = 250) +
    stat_bin(aes(y = after_stat(density), color = NULL), position = "identity", geom = "line", bins = 250) +
    scale_x_continuous(label = scales::label_number(scale_cut = scales::cut_si("s"))) +
  	scale_color_hue(h.start = 70) +
  	facet_grid(rows = vars(type)) +
    labs(x = "Duration", y = "Density", color = "Path", fill = "Path")
	plot

  return(plot)
}

.vis_compare_path_histline_annotated <- function(df) {
	extr_values <- df |>
		dplyr::select(max, q99, q95, q90, type) |>
		unique() |>
		pivot_longer(cols = !c(type))
  plot <- df |>
    .vis_compare_path_histline() +
    geom_vline(data = extr_values, aes(xintercept = value, color = type, linetype = name), show.legend = F) +
    geom_text(data = extr_values, aes(x = value, y = 1, label = scales::label_number(scale_cut = scales::cut_si("s"))(value)), show.legend = F, check_overlap = TRUE)

  return(plot)
}

.vis_compare_path_histline_log <- function(df) {
  plot <- df |>
    .vis_compare_path_histline() +
    scale_x_log10(label = scales::label_number(scale_cut = scales::cut_si("s")))

  return(plot)
}

.vis_compare_path_histline_log_annotated <- function(df) {
  plot <- df |>
    .vis_compare_path_histline_annotated() +
    scale_x_log10(label = scales::label_number(scale_cut = scales::cut_si("s")))

  return(plot)
}

.vis_compare_path_histline_q99 <- function(df) {
  plot <- df |>
    filter(duration >= q99) |>
    .vis_compare_path_histline() +
    geom_vline(aes(xintercept = max, color = type, linetype = "max"))

  return(plot)
}

