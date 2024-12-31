if (!exists("path.src")) {
  abort("Object not found")
}

message(sprintf("! Block Size = %d", block_size))

path.bm <<- pick_block_maxima(path.src, block_size, by = c(context))

# path.bm <- path.bm |> filter(n() > 10, .by = all_of(c("path", context)))

path.bm |>
  ggplot(aes(x = duration)) +
  stat_bin(aes(y = after_stat(density))) +
  facet_grid(rows = vars(.data[[context]]), cols = vars(path))

# 1 SERIES OF MAXIMA
path.bm |>
  ggplot(aes(x = run, y = duration, color = path)) +
  scale_y_continuous("Duration", labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
  scale_x_continuous("Run ID", labels = NULL, limits = c(0, NA)) +
  geom_point(show.legend = F) +
  geom_smooth(method = "loess", formula = y ~ x, color = "black") +
  # facet_grid(rows = context) +
  facet_wrap(c("path"), scales = "fixed", axis.labels = "margins") +
  guides(color = "none") +
  geom_label(
    data = . %>% summarise(count = n(), .by = all_of(c("path"))),
    aes(x = -Inf, y = -Inf, label = sprintf("n=%d", count)),
    hjust = -0.3,
    vjust = -0.3,
    color = "black",
    alpha = 0.7,
  )
save_for_thesis("path-bm-timeline", full_page = T)


# 2 HISTOGRAM
{
  path.bm |>
    ggplot(aes(x = duration, y = after_stat(count))) +
    scale_y_continuous("Count") +
    scale_x_continuous("Duration", labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    stat_bin(aes(fill = path), color = "black", geom = "bar", bins = 45, alpha = .8, show.legend = F) +
    facet_wrap(
      c("path"),
      scales = "free_y",
      axis.labels = "margins"
    )

  save_for_thesis("path-bm-histograms")

  path.bm |>
    ggplot(aes(x = duration, y = after_stat(count))) +
    scale_y_continuous("Count") +
    scale_x_continuous("Duration", labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    stat_bin(bins = 120, color = "black", fill = NA, geom = "bar")

  save_for_thesis("path-bm-histogram")
}


# 3 QUANTILE-QUANTILE → quality of the fit is reliable
if (F) {
  path.bm |>
    mutate(path = as.factor(path) |> as_integer() |> as.factor()) |>
    ggplot() +
    scale_y_continuous("Count") +
    scale_x_continuous("Duration", labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    stat_binline(aes(x = duration, y = after_stat(count)), bins = 40)

  path.bm |>
    mutate(path = as.factor(path) |> as_integer() |> as.factor()) |>
    ggplot(aes(sample = duration, color = path)) +
    stat_qq_band() +
    stat_qq_line() +
    stat_qq_point() +
    facet_wrap(vars(path), scales = "free_x") +
    labs()
}

