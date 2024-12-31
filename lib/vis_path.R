vis_path <- function(df) {
  save <- function(plot_name, ...) {
    save_func(paste0(data_type, "path-", plot_name))
  }

  required_columns <- c("path", duration)
  stopifnot(all(required_columns %in% colnames(df)))

  plots <- list()

  meta_paths <- path_information(df)

  plot_name <- ""
  plots[[plot_name]] <- df |>
    ggplot(aes(x = duration)) +
    stat_bin(aes(y = after_stat(ndensity))) +
    scale_x_continuous(label = scales::label_number(scale_cut = scales::cut_si("s"))) +
    geom_label(data = meta_paths, aes(x = 0, y = 1, label = scales::percent(frequency))) +
    facet_wrap(~path) +
    labs(y = "Density [a.u.]", x = "Duration")
  save(plot_name)

  return(plots)
}
