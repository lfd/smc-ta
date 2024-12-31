data_load_cached("path.log")
rm(var)

path.log |>
  summarise(
    n = n(),
    max = max(duration),
    npath = length(unique(path)),
    .by = all_of(context)
  )

filter(path.log, cpu_id == 2) |> first()

path.log |>
  filter(cpu_id == 1) |>
  filter(duration >= quantile(duration, 0.99)) |>
  filter(path == "expected→2→6→4→3→1→real") |>
  nrow()

path.log |>
  filter(duration >= quantile(duration, 0.99)) |>
  summarise(
    max = max(duration), n = n(),
    # .by = all_of(context)
  )


.maxs <- path.src |>
  summarise(max = max(duration), .by = c(cpu_id))

path.src |>
	summarise(
		y0 = min(duration),
		y01 = quantile(duration, 0.01),
		y10 = quantile(duration, 0.1),
		y50 = mean(duration),
		y90 = quantile(duration, 0.9),
		y99 = quantile(duration, 0.99),
		y100 = max(duration),
		.by = c(cpu_id)
	) |>
  ggplot() +
  geom_boxplot(aes(ymin = y01, lower = y10, middle = y50, upper = y90, ymax = y99, x = cpu_id), stat="identity") +
	geom_point(data = .maxs, aes(x = cpu_id, y = max), size = 2) +
	geom_label(data = .maxs, aes(x = cpu_id, y = max, label = paste0(round(max * 1e6, 1), " \u00B5s ")), hjust=-0.1) +
  scale_y_continuous("Latency [µs]", labels = \(x) x * 1e6) +
  scale_x_discrete("CPU ID") +
	theme_minimal()
save_for_thesis("context-boxplot", prop = 0.6)
