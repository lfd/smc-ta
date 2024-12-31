plot_transition <- function(data, from, to, sim) {
  save <- function(plot_name, ...) {
    save_func(paste0(data_type, "transition-duration-", plot_name))
  }

  plot_name <- paste(from, to, sep = "--")
  plot <- data %>%
    dplyr::filter(event.from == from, event.to == to) %>%
    # dplyr::pull(duration) %>%
    ggplot(aes(x = duration)) +
    stat_density(aes(y = after_stat(ndensity), fill = "real")) +
    stat_density(data = data.frame(x = sim), aes(x = x, y = after_stat(ndensity), fill = "fit")) +
    scale_x_log10() +
    scale_fill_manual(values = alpha(c("blue", "red"), .3)) +
    labs(title = transition(from, to))
  save(plot_name)

  return(plot)
}
