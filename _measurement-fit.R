
prop <- 0.5

{
  binwidth <- ifelse(exists("binwidth"), binwidth, 1e-6)
  path.cmp |>
    ggplot(aes(x = duration, fill = source)) +
    stat_bin(aes(y = after_stat(count), group = source), position = "dodge",
             binwidth = binwidth,
             bins = 100,
             ) +
    scale_x_continuous("Latency [µs]", labels = \(x) x * 1e6) +
    scale_fill_viridis_d() +
    labs(y = "Count", fill = "") +
    theme_minimal() +
    theme(
      legend.position = c(0.95, .8)
    )
  save_for_thesis("measurement-fit-histogram", prop = 0.4)
}

{
  q <- 0.9
  q <- 0.99
  q <- 0.999
  q <- 0.9999
  binwidth <- ifelse(exists("binwidth"), binwidth, .5e-6)

  lim <- quantile(path.src$duration, q)


  .data <- path.cmp |>
    filter(duration >= ceiling(lim / binwidth) * binwidth)

  .data |>
    summarise(
      max = max(duration),
      n = n(),
      .by = c(path)
    ) |>
    arrange(desc(max))

  .data |>
    ggplot(aes(x = duration, fill = source)) +
    stat_bin(aes(y = after_stat(count), group = source), position = "dodge", binwidth = binwidth) +
    scale_x_continuous("Latency [µs]", labels = \(x) x * 1e6) +
    scale_fill_viridis_d() +
    labs(y = "Count", fill = "") +
    theme_minimal() +
    theme(
      legend.position = c(0.9, 0.7)
    )
  filename <- paste0("measurement-fit-tail", round(log10(1-q), 0), "-histogram")
  save_for_thesis(filename, prop = prop)
}

{
	require(ggh4x)
  .data <- path.cmp %>%
    {
      by <- c("source")
      bind_rows(
        calc_quantiles(., by = by, probs = probs) |> mutate(name = ordered(scales::label_percent(accuracy = 1e-3)(name))),
        summarise(., value = max(duration), name = "max", .by = by)
      )
    }
  .d <- .data |> tidyr::pivot_wider(names_from = c("source"), values_from = c("value"))
  .data |>
    ggplot(aes(x = interaction(ordered(source), name, sep = "!"), y = value, color = source)) +
    # geom_line(aes(group = source))+
    geom_point(size = 2) +
  	geom_segment(data = .d, aes(x = paste("Sampling", name, sep="!"), y = Measurement, yend=Sampling, color = NULL), color = "gray60") +
  	geom_text(data = .d, aes(x = paste("Sampling", name, sep="!"), y = Measurement, label = paste0(scales::label_number(style_positive = "plus")(round((Sampling- Measurement) * 1e6, 1)), " µs"), color = NULL),show.legend = F,
  	          #hjust = -1,
  	          vjust = -1,
  	) +
  	geom_text(data = .d, aes(x = paste("Sampling", name, sep="!"), y = Measurement, label = paste0(scales::label_percent(style_positive = "plus")(round(((Sampling- Measurement) / Measurement), 3))), color = NULL), show.legend = F,
  	          #hjust = 1,
  	          #nudge_x = 0.2,
  	          vjust = 1,
  	          nudge_y = -.05e-6
  	          ) +
  	geom_segment(data = .d, aes(x = paste("Measurement", name, sep="!"), y = Measurement, xend = paste("Sampling", name, sep="!"), color = NULL), color = "gray60") +
    scale_x_discrete("Quantiles and WCET", guide = guide_axis_nested(delim = "!"), expand = expansion(add = 1)) +
    scale_y_continuous("Latency [µs]",
    									 labels = \(x) x * 1e6,
    									 #limits = c(18e-6, NA),
    									 expand = expansion(add = 0.2e-6)
    									 ) +
    scale_color_viridis_d("") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      ggh4x.axis.nesttext.x = element_text(angle = 35, hjust = 0.5),
      ggh4x.axis.nestline.x = element_line(),
      axis.title.x = element_blank(),
      legend.position = c(0.9, 0.25)
    )
  save_for_thesis("measurement-fit-estimation", prop = prop)
}
