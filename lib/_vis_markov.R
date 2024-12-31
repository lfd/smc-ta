
plot_transition_fit <- function(real, fit, save_func = function(...) {}, data_type = NULL) {
  save <- function(plot_name, ...) {
    save_func(paste0(data_type, "markov-", plot_name))
  }

  plots <- list()

  plot_name <- "compare"
	plots[[plot_name]] <- ggplot() +
		stat_bin(aes(x = real, y = after_stat(ndensity),fill = "real"), alpha = .4) +
		stat_density(aes(x = real, y = after_stat(ndensity), color = "real"), alpha = 0) +
		stat_bin(aes(x = fitted, y = after_stat(ndensity), fill = "fitted"), alpha = .4) +
		stat_density(aes(x = fitted, y = after_stat(ndensity), color = "fitted"), alpha = 0) +
		scale_x_continuous(name = "Duration", labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
		scale_fill_discrete(name = NULL) +
		scale_color_discrete(guide = NULL) +
		theme(legend.position = "right")
	save(plot_name)

	return(plots)
}
