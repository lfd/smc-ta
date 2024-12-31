if (!exists("path.src")) {
  abort("Object not found")
}

message(sprintf("! %d Durchläufe", nrow(path.src)))
path.src |>
  summarise(
    n = n(),
    .by = path
  ) |>
  mutate(
    freq = n / sum(n),
    c_freq = scales::label_percent()(freq)
  )

{
  path.src |>
    mutate(c_path = forcats::fct_relabel(path, \(x) paste("Path", x))) |>
    ggplot(aes(x = duration)) +
    stat_bin(
      aes(fill = path),
      binwidth = 0.5e-6,
      #color = "black",
      #fill = NA
      show.legend = F
    ) +
    stat_bin(
      aes(fill = path),
      binwidth = 0.5e-6,
      #color = "black",
      #fill = NA
      geom = "line",
      show.legend = F
    ) +
    scale_x_continuous("Latency [µs]", labels = \(x) x * 1e6) +
    labs(y = "Count") +
    facet_wrap(
      vars(c_path),
      scales = "free_y",
      ncol = 3,
    ) +
    theme_minimal()
  save_for_thesis("path-histogram", prop = 0.4)
  # save_for_thesis("path-histogram", full_page = T)
}
