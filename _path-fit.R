if (!exists("trans.src")) {
  abort("Object not found")
} #
if (!exists("mapping")) {
  abort("Object not found")
}

N <- unique(trans.src$run) |> length()

{
  data <- trans.src |>
    pick_block_maxima(block_size = block_size, by = c("event.from", "event.to", context))
  Q <- method_fit(data, purrr::chuck(method, "fit_fun"))
  path.fit <- data |>
    evaluate_method(
      N,
      simulation_config = list(
        start_events = start_events,
        end_events = end_events,
        # P1 = P1,
        sample_fun = purrr::chuck(method, "sample_fun")
      ),
      fit_fun = purrr::chuck(method, "fit_fun"),
      Q = Q,
      return.type = "path"
    )

  rm(
    # data,
    Q
  )
}

dump_var("path.fit", DATA_DIR)

data_load_cached("path.fit")

data_load_cached("path.log")
rm(var)

path.src <- path.log |>
  filter(cpu_id == 1)
rm(path.log)

cols <- c("path", "duration", "run")

.paths <- unique(path.cmp$path)
.paths[!(.paths %in% names(mapping))]
stopifnot(all(unique(path.cmp$path) %in% names(mapping)))

path.cmp <- bind_rows(
  select(path.fit, all_of(cols)) |> mutate(source = "Sampling"),
  select(path.src, all_of(cols)) |> mutate(source = "Measurement"),
)

{
  # unobserved paths
  new_paths <- unique(path.fit$path)
  new_paths <- new_paths[!(new_paths %in% names(mapping))]
  missing_paths <- names(mapping)[!(names(mapping) %in% unique(path.fit$path))]
  mapping[missing_paths] |>
    unlist() |>
    unname()

  path.fit |>
    filter(!path %in% new_paths) |>
    nrow()

  require(ggbreak)

  path.fit |>
    mutate(
      unobserved = path %in% new_paths,
      label = ordered(unobserved, levels = c(TRUE, FALSE), c("new", "known"))
    ) |>
    ggplot(aes(x = duration)) +
    stat_bin(aes(fill = label), binwidth = 0.1e-5, position = "stack") +
    scale_x_continuous(labels = \(x) x * 1e6) +
    # scale_y_continuous("Count") +
    # coord_cartesian(ylim = c(0, 8000)) +
    labs(fill = "Path") +
    scale_y_cut(c(2100, 8300)) +
    labs(x = "Latency [µs]", y = "Count") +
    theme_minimal() +
    theme(legend.position = "top")
  save_for_thesis("path-fit-new-path-histogram", prop = 0.6)

  path.fit |>
    # filter(path %in% new_paths) |>
    summarise(
      n = n(),
      max = max(duration),
      "90.0%" = quantile(duration, 0.90),
      "99.0%" = quantile(duration, 0.99),
      "99.9%" = quantile(duration, 0.999),
      .by = path
    ) |>
    tidyr::pivot_longer(cols = starts_with("9")) |>
    mutate(
      length = str_count(path, "→"),
      path = factor(path)
    ) |>
    ggplot() +
    geom_jitter(aes(x = length, y = value, size = n, color = name), height = 0) +
    geom_smooth(aes(x = length, y = value, color = name, weight = n), method = "lm", show.legend = F) +
    scale_y_continuous("Latency [µs]", labels = \(x) x * 1e6) +
    scale_x_continuous("Path Lenth") +
    scale_color_discrete("Quantile") +
    scale_size("Occurrences") +
    # scale_color_viridis_d("Quantile") +
    facet_grid(rows = vars(name))
}

{
  binwidth <- ifelse(exists("binwidth"), binwidth, 0.1e-6)
  path.cmp |>
    mutate(path = mapping[path]) |>
    mutate(path = factor(paste("Path", path))) |>
    ggplot(aes(x = duration, fill = source)) +
    stat_bin(aes(y = after_stat(count), group = source), position = "dodge", binwidth = 0.5e-6) +
    scale_x_continuous("Latency [µs]", labels = \(x) x * 1e6) +
    scale_fill_viridis_d() +
    labs(y = "Count", fill = "") +
    facet_wrap(
      vars(path),
      scales = "free_y"
    ) +
    theme_minimal()
  save_for_thesis("path-fit-histogram", prop = 0.3)
}

{
  require(ggh4x)
  path.cmp %>%
    {
      by <- c("path", "source")
      bind_rows(
        calc_quantiles(., by = by, probs = probs) |> mutate(name = ordered(scales::label_percent(accuracy = 1e-3)(name))),
        summarise(., value = max(duration), name = "max", .by = by)
      )
    } %>%
    mutate(path = paste("Path", path)) |>
    ggplot(aes(x = interaction(ordered(source), name, sep = "!"), y = value, color = source)) +
    geom_point(size = 2) +
    # geom_line(aes(group = source))+
    scale_x_discrete("Quantiles and WCET", guide = guide_axis_nested(delim = "!")) +
    scale_y_continuous("Latency [µs]", labels = \(x) x * 1e6) +
    facet_wrap(vars(path)) +
    scale_color_viridis_d("") +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      ggh4x.axis.nesttext.x = element_text(angle = 35, hjust = 0.5),
      axis.title.x = element_blank()
    )
  save_for_thesis("path-fit-estimation", prop = 0.5)
}
