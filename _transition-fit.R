if (!exists("trans.src")) {
  abort("Object not found")
}

trans.log <- trans.src

{
  by <- c("event.from", "event.to", context)
  trans.src <- trans.log |>
    pick_block_maxima(block_size = block_size, by = by)
  Q <- method_fit(trans.src, purrr::chuck(method, "fit_fun")) |>
    mutate(transition = paste_trans(event.from, event.to))

  counts <- trans.log |>
    summarise(count = n(), .by = all_of(by))

  trans.fit <- left_join(Q, counts, by = by) |>
    ungroup() |>
    reframe(
      duration = nor1mix::rnorMix(count, params[[1]]),
      .by = all_of(by)
    )
}

dump_var("trans.fit", DATA_DIR)
data_load_cached("trans.fit")
rm(var)

accuracy <- (1.0 - max(probs)) * 1e2

cols <- c("event.from", "event.to", "duration", context)
trans.cmp <- bind_rows(
  select(trans.fit, all_of(cols)) |> mutate(source = "Sampling"),
  select(trans.src, all_of(cols)) |> mutate(source = "BMM"),
  select(trans.log, all_of(cols)) |> mutate(source = "Measurement"),
) |>
  filter(!is.na(duration)) |>
  mutate(transition = paste_trans(event.from, event.to))

{
  binwidth <- ifelse(exists("binwidth"), binwidth, 0.1e-6)
  trans.cmp |>
    ggplot() +
    stat_bin(aes(x = duration, y = after_stat(count), fill = source), binwidth = binwidth, position = "dodge") +
    scale_x_continuous("Transition Duration [µs]", labels = \(x) x * 1e6) +
    scale_y_continuous("Count") +
    # theme(legend.position = "right") +
    scale_fill_viridis_d("") +
    facet_wrap(vars(transition), scales = "free", ncol = 3) +
    theme_minimal() +
    theme(
      legend.position = "bottom"
    )
  save_for_thesis("transition-fit-histogram", prop = 0.6)
  save_for_thesis("transition-fit-histogram", full_page = T)
}

{
  binwidth <- ifelse(exists("binwidth"), binwidth, 0.1e-6)
  trans.cmp %>%
    {
      bind_rows(
        calc_quantiles(., by = c("transition", "source"), probs = probs) |> mutate(name = ordered(scales::label_percent(accuracy = 1e-3)(name))),
        summarise(.,
          value = max(duration),
          name = "max",
          .by = c("transition", "source")
        )
      )
    } %>%
    ggplot(aes(x = interaction(ordered(source), name, sep = "!"), y = value, color = source)) +
    geom_point(size = 2) +
    # geom_line(aes(group = source))+
    scale_x_discrete("Quantiles and WCET", guide = guide_axis_nested(delim = "!")) +
    scale_y_continuous("Latency [µs]",
                       labels = \(x) x * 1e6,
                       #breaks = seq(0,100e-6,binwidth)
                       ) +
    facet_wrap(vars(transition),
               scales = "free",
               ncol = 3
               ) +
    scale_color_viridis_d("")  +
    theme_minimal()+
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_line(),
      ggh4x.axis.nestline.x = element_line(),
      ggh4x.axis.nesttext.x = element_text(angle = 35, hjust = 0.5),
      axis.title.x = element_blank(),
      legend.position = "bottom"
    )

  save_for_thesis("transition-fit-estimation", prop = 0.6)
  save_for_thesis("transition-fit-estimation", full_page = T)
}
