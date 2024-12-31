source("jittertest-ttp.R")

require(ggplot2)

tibble(
	bucket = c(25, 26, 27) * 1e-6,
	annotated = c(28548, 1452,0),
	blank = c(29999, 0,1)
) |>
	tidyr::pivot_longer(c("annotated", "blank")) |>
	ggplot(aes(x = bucket, y = value, fill = name, color = name)) +
	geom_bar(stat = "identity", position = "dodge",na.rm = TRUE) +
  scale_x_continuous("Latency [µs]", labels = \(x) x * 1e6) +
	geom_label(aes(label = value,
								hjust = if_else(name == "blank", "left", "right")
								),
						 fill = "white",
						 color = "black",
						#label.padding =  unit(0.55, "lines")
						) +
	scale_colour_viridis_d() +
	scale_fill_viridis_d() +
	labs(y = "Count", color = "Setup", fill = "Setup") +
	theme(legend.position = "right")

save_for_thesis("overhead-histogram", prop = 0.4)
