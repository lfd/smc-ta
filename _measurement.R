if (!exists("path.src")) {
  abort("Object not found")
}

prop <- 0.6

{
  binwidth <- ifelse(exists("binwidth"), binwidth, .1e-6)
  path.src |>
    ggplot(aes(x = duration, y = after_stat(count))) +
    scale_x_continuous(
      "Duration [µs]",
      # labels = scales::label_number(scale_cut = scales::cut_si("s")),
      labels = \(x) x * 1e6
    ) +
    stat_bin(
      aes(fill = path),
      binwidth = binwidth,
      color = "black",
      fill = NA,
      geom = "bar"
    ) +
    labs(y = "Count") +
    theme_minimal()
  save_for_thesis("measurement-histogram", prop = 0.6)
}

{
  q <- 0.99
  q <- 0.999
  q <- 0.9999
  binwidth <- .5e-6

  lim <- quantile(path.src$duration, q)


  .data <- path.src |>
    mutate(path = ordered(mapping[path], levels=mapping)) |>
    filter(duration >= ceiling(lim / binwidth) * binwidth)

  .data |>
    summarise(
      max = max(duration),
      n = n(),
      .by = c(path)
    ) |>
    arrange(desc(max))

  .data |>
    ggplot(aes(x = duration, y = after_stat(count))) +
    scale_y_continuous("Count") +
    scale_x_continuous(
      "Duration [µs]",
      limits = c(lim, NA),
      # labels = scales::label_number(scale_cut = scales::cut_si("s")),
      labels = \(x) x * 1e6
    ) +
    # stat_bin(binwidth = .5e-6, color = "black", fill = NA, geom = "bar")
    stat_bin(aes(fill = path), binwidth = .5e-6, color = "black", geom = "bar") +
    theme_minimal() +
    labs(fill = "Path") +
    theme(legend.position=c(.95,0.55))

  filename <- paste0("measurement-tail", log10(1 - q), "-histogram")
  save_for_thesis(filename = filename, prop = 0.65)
}
