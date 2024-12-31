#' Visualize Transition Probabilities
#'
#' This function is used for visualizing transition probabilities in a given data frame.
#' It creates a raster plot of transition probabilities and optionally saves the plot using a specified saving function.
#'
#' @param df The data frame containing transition probability information.
#' @param save_func (optional) A user-defined function to save the generated plot.
#' If not provided, the plot is not saved.
#'
#' @return This function returns the transition probability plot.
#'
#' @export
vis_probs <- function(df, save_func = function(...) {}, data_type = NULL) {
  save <- function(plot_name, ...) {
    save_func(paste0(data_type, "transition-probs-", plot_name))
  }

  plots <- list()

  plot_name <- "tile"
	plots[[plot_name]] <- .vis_probs_raster(df)
	save(plot_name)

	return(plots)
}

.vis_probs_raster <- function(df) {
	plot <- df |>
		ggplot(aes(y = event.from, x = event.to, z = probability)) +
		geom_tile(aes(fill = probability), color = "black", linewidth = .3) +
		geom_label(aes(label = scales::label_percent()(probability))) +
		scale_x_discrete("State To") +
		scale_y_discrete("State From") +
		scale_fill_gradient("Probability") +
		#scale_color_gradient(NULL, low = "white", high = "black") +
		theme(
			legend.position = "right",
			panel.grid.major.y = element_blank(),
			panel.grid.major.x = element_blank()
		)

	return(plot)
}


.vis_prob_count <- function(df) {
	events <- unique(c(df$event.from, df$event.to)) |> droplevels() |> purrr::keep(\(x)!is.na(x))
	plot <- tidyr::crossing(event.from = events, event.to = events) |>
		filter(event.from != event.to) |>
		left_join(summarise(df, n = n(), .by = c(event.from, event.to)), by = c("event.from", "event.to")) |>
		ggplot(aes(y = event.from, x = event.to, z = n)) +
		geom_tile(aes(fill = n), color = "black", linewidth = .3) +
		geom_tile(data = tibble(event.from = events, event.to = events), aes(z = NULL), fill = "black", color = "black", linewidth = .3) +
		geom_label(aes(label = scales::label_number()(n))) +
		scale_x_discrete("To") +
		scale_y_discrete("From") +
		scale_fill_viridis_c("Count") +
		theme_minimal() +
		theme(
			legend.position = "top",
			legend.key.width = unit(15, "mm"),
			#panel.grid.major.y = element_blank(),
			#panel.grid.major.x = element_blank(),
			panel.grid.minor = element_blank()
		)
	plot
	#save_for_thesis("transition-count", prop = 0.4)

	return(plot)
}

