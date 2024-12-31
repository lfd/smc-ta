if (!exists("trans.src")) {
  abort("Object not found")
}

with_progress({
  p <- progressr::progressor(steps = 2 * nrow(combinations))
  plan(multisession)

  # p <- message
  # sampled.path <- purrr::pmap(
  sampled.path <- furrr::future_pmap(
    combinations,
    function(size, method_name, method, block_size, N, ...) {
      if (size > max(trans.src$run)) {
        warn(sprintf("size '%d' exceeds number of runs present in the data"))
        return(NULL) # SKIP
      }

      p(sprintf("Start %s for N=%d (block size=%d)", method_name, size, block_size))

      # Evaluate the method
      data.org <- trans.src |>
        data_select(size, randomized = TRUE)
      data <- data.org |>
        pick_block_maxima(block_size, by = c("event.from", "event.to", context))
      message("Data prepared")
      Q <- method_fit(data, purrr::chuck(method, "fit_fun"))
      result <- data |>
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
        ) |>
        mutate(method = factor(method_name), N = size, shots = N)
      message("Finished evaluting")

      # saveRDS(result, file = file.path(DATA_DIR, paste0(paste("sampled", "path", size, method_name, block_size, N, sep = "-"), ".rds")))

      .max <- filter(result, duration == max(duration))[1, ]

      agg <- list(
        # result = result,
        data = data.org |> conv_transition_to_path.dt(),
        quantiles = calc_quantiles(result, c("method"), probs = probs),
        maxi = .max$duration,
        max_path = .max$path,
        Q = list(Q),
        meta = list(count = nrow(data), size = size, method = method_name, block_size = block_size, N = N)
      )
      p(sprintf("Finish %s for N=%d (block size=%d)", method_name, size, block_size))
      return(agg)
    },
    .options = furrr_options(
      stdout = T, scheduling = T, globals = T, seed = 229412,
    )
  )

  plan(sequential)
})


dump_var("sampled.path", DATA_DIR)
write.csv(sampled.path, file.path(DATA_DIR, "sampled.path.csv"))



data_load_cached("sampled.path")

sampled.path |>
  purrr::keep(\(x) purrr::chuck(x, "meta", "block_size") == 10) |>
  purrr::keep(\(x) purrr::chuck(x, "meta", "method") == "nmixture4") |>
  purrr::map(~ .$meta)



.prepare_plotting <- function(df) {
  mutate(
    df,
    count = size %/% block_size,
    f_count = factor(count, ordered = T),
    f_block_size = factor(block_size,
      levels = as.character(unique(h_block_sizes) |> sort() |> rev()),
      ordered = T
    ),
    rel_size = round(size / max(trans.src$run), digits = 3),
    f_rel_size = factor(rel_size, ordered = T),
    c_rel_size = factor(rel_size,
      levels = as.character(unique(rel_size) |> sort()),
      labels = scales::label_percent(accuracy = 0.1)(unique(rel_size) |> sort()),
      ordered = T
    ),
    f_size = factor(size, ordered = T),
  )
}

{
  data_quantiles <- purrr::map(sampled.path, function(x) {
    purrr::chuck(x, "data") |>
      calc_quantiles(by = NULL, probs = probs) |>
      mutate(
        count = purrr::chuck(x, "meta", "count"),
        size = purrr::chuck(x, "meta", "size"),
        method = purrr::chuck(x, "meta", "method"),
        block_size = purrr::chuck(x, "meta", "block_size"),
        N = purrr::chuck(x, "meta", "N")
      )
  }) |>
    rbindlist()

  data_maxis <- purrr::map(sampled.path, function(x) {
    purrr::chuck(x, "data") |>
      summarise(maxi = max(duration), by = NULL) |>
      mutate(
        count = purrr::chuck(x, "meta", "count"),
        size = purrr::chuck(x, "meta", "size"),
        method = purrr::chuck(x, "meta", "method"),
        block_size = purrr::chuck(x, "meta", "block_size"),
        N = purrr::chuck(x, "meta", "N")
      )
  }) |>
    rbindlist()
}

{
  data_load_cached("path.log")

  path.src <- path.log |>
    # filter(cpu_id == 1) |>
    mutate(run = cur_group_id(), .by = all_of(c("run", context)))
  base_maxi <<- max(path.src$duration)

  base_quantiles <- calc_quantiles(path.src, NULL, probs = probs)

  data_maxis <<- h_sizes |>
    purrr::map(
      function(size) {
        message(size)
        data_select(path.src, size, randomized = F) |>
          summarise(maxi = max(duration), size = size)
      }
    ) |>
    rbindlist()

  data_quantiles <<- h_sizes |>
    purrr::map(
      function(size) {
        message(size)
        data_select(path.src, size, randomized = F) |>
          calc_quantiles(by = NULL, probs = probs) |>
          mutate(size = size)
      }
    ) |>
    rbindlist()

  rm(path.log)
}


data_maxis <- data_maxis |>
  mutate(
    block_size = 1,
    value = maxi,
    .keep = "unused"
  ) |>
  .prepare_plotting() |>
  mutate(
    block_size = NA,
    f_block_size = NA
  )
data_maxis


data_quantiles <- data_quantiles |>
  mutate(
    block_size = 1,
    .keep = "unused"
  ) |>
  .prepare_plotting() |>
  mutate(
    block_size = NA,
    f_block_size = NA
  ) |>
  mutate(
    f_name = factor(name, ordered = T),
    c_name = scales::label_percent()(name),
  )
data_quantiles


maxis <- purrr::map(sampled.path, function(x) {
  tibble(
    meta = purrr::chuck(x, "meta") |> list(),
  ) |>
    tidyr::unnest_wider("meta") |>
    mutate(maxi = x$maxi)
}) |>
  rbindlist() |>
  mutate(value = maxi, .keep = "unused") |>
  .prepare_plotting() |>
  mutate(
    name = 1,
    f_name = factor(name, c(probs, 1), ordered = T),
    c_name = scales::label_percent()(name),
  )

quantiles <- purrr::map(sampled.path, function(x) {
  tibble(
    meta = purrr::chuck(x, "meta") |> list(),
  ) |>
    tidyr::unnest_wider("meta") |>
    right_join(x$quantiles, by = "method")
}) |>
  rbindlist() |>
  .prepare_plotting() |>
  mutate(
    f_name = factor(name, ordered = T),
    c_name = scales::label_percent()(name),
  )




require(ggplot2)
require(cowplot)
require(ggh4x)


### Heatmap

maxis |>
  # ggplot(aes(x = c_rel_size, y = f_block_size)) +
  ggplot(aes(
    y = c_rel_size,
    x = f_block_size
  )) +
  geom_raster(aes(fill = f_count)) +
  geom_label(aes(label = f_count), show.legend = F) +
  labs(
    y = "Data Size",
    x = "Block Size"
  ) +
  guides(fill = "none") +
  theme_minimal()
save_for_thesis("path-fitted-count-board")


#### MAXIMA

maxis |>
  ggplot(aes(x = count, y = value)) +
  geom_point(aes(color = c_rel_size)) +
  geom_line(aes(group = size, color = c_rel_size)) +
  geom_hline(aes(yintercept = base_maxi), linetype = "dotted", color = "black") +
  geom_label(data = NULL, aes(x = Inf, y = base_maxi, label = paste("data max:", base_maxi * 1e6)), hjust = 1.1) +
  scale_x_log10() +
  scale_y_continuous("WCET [µs]", labels = \(x) x * 1e6) +
  facet_grid(rows = vars(method), scales = "free")

maxis |>
  ggplot(aes(x = interaction(f_block_size, c_rel_size), y = value)) +
  scale_x_discrete("Data Usage", guide = "axis_nested") +
  scale_y_continuous("WCET [µs]", labels = \(x) x * 1e6) +
  geom_line(
    data = data_maxis |>
      left_join(maxis, by = c("size"), suffix = c("", ".y")) |>
      filter(!is.na(method)),
    aes(x = interaction(f_block_size.y, c_rel_size), y = value, group = c_rel_size),
    color = "gray50",
    show.legend = F
  ) +
  geom_point(aes(color = f_block_size)) +
  geom_line(
    aes(group = c_rel_size, ),
    show.legend = F,
    color = "gray20"
  ) +
  scale_color_discrete("Block Size") +
  theme(
    axis.text.x = element_blank(),
    ggh4x.axis.nesttext.x = element_text()
  ) +
  facet_grid(rows = vars(method)) +
  theme_linedraw() +
  theme(legend.position = "bottom")
save_for_thesis("path-fitted-maxs")


.methods <- unique(maxis$method)
purrr::map(
  .methods,
  function(x) {
    maxis |>
      filter(method == x) |>
      ggplot(aes(x = f_block_size, y = value)) +
      scale_x_discrete("Block Size") +
      scale_y_continuous("WCET [µs]", labels = \(x) x * 1e6) +
      geom_line(
        data = data_maxis |>
          left_join(maxis, by = c("size"), suffix = c("", ".y")) |>
          filter(!is.na(method)),
        aes(x = f_block_size.y, y = value, group = c_rel_size),
        color = "gray50",
        show.legend = F
      ) +
      geom_point(aes(color = f_block_size)) +
      geom_line(
        aes(group = c_rel_size, ),
        show.legend = F,
        color = "gray20"
      ) +
      scale_color_discrete("Block Size") +
      guides(color = "none") +
      theme_minimal() +
      facet_wrap(vars(c_rel_size), scales = "fixed")
  }
) %>%
  plot_grid(plotlist = ., labels = .methods, hjust = 0, ncol = 1)
save_for_thesis("path-fitted-maxs-grid", full_page = T)


### QUANTILES

quantiles |>
  ggplot(aes(x = count, y = value)) +
  geom_point(aes(shape = f_block_size)) +
  # geom_line(aes(group = size, color = size)) +
  scale_x_log10() +
  # scale_x_discrete(guide = "axis_nested") +
  scale_y_continuous("WCET [µs]", labels = \(x) x * 1e6) +
  facet_grid(
    cols = vars(method),
    rows = vars(name),
  )

quantiles |>
  ggplot(aes(x = f_block_size, y = value, color = c_name)) +
  geom_jitter(width = 0.1) +
  scale_x_discrete("Block Size", guide = "axis_nested") +
  scale_y_continuous("WCET [µs]", labels = \(x) x * 1e6) +
  scale_color_viridis_d("Quantiles") +
  facet_grid(cols = vars(method), rows = vars(c_rel_size), scales = "free_y") +
  theme(legend.position = "bottom")
save_for_thesis("path-fitted-quantiles", full_page = T)








#### Simplified: if only one block_size and one method


rel_sizes_selection <- seq.int(1, 9) / 10
rel_sizes_selection <- c(seq.int(1, 9, 3), seq.int(1, 9, 3) * 10, 100) / 100
rel_sizes_selection <- c(seq.int(1, 10), 100, 1000) / 1000
rel_sizes_selection <- c(2, 5, 8, 20, 50, 80, 200, 500, 1000) / 1000
rel_sizes_selection <- c(1, 4, 7, 10, 40, 70, 100, 400, 700) / 1000


all_quantiles <- tidyr::crossing(base_quantiles, tibble(size = h_sizes)) |>
  mutate(
    block_size = 1,
    .keep = "unused"
  ) |>
  .prepare_plotting() |>
  mutate(
    block_size = NA,
    f_block_size = NA
  ) |>
  mutate(
    f_name = factor(name, ordered = T),
    c_name = scales::label_percent()(name),
  )
all_quantiles

all_maxi <- tidyr::crossing(tibble(maxi = base_maxi, name = 1), tibble(size = h_sizes)) |>
  mutate(
    block_size = 1,
    value = maxi,
    .keep = "unused"
  ) |>
  .prepare_plotting() |>
  mutate(
    block_size = NA,
    f_block_size = NA
  )
all_maxi

data_quantiles |>
  ggplot(aes(x = c_rel_size, y = value, color = c_name)) +
  geom_point()


ggplot(mapping = aes(
  x = c_name,
  # x = rel_size,
  y = value
)) +
  geom_point(data = filter(all_quantiles, rel_size %in% rel_sizes_selection), aes(color = "All Data"), size = 2) +
  geom_point(data = filter(all_maxi, rel_size %in% rel_sizes_selection) |> mutate(c_name = "WCET"), aes(color = "All Data"), size = 2) +
  geom_point(data = filter(quantiles, rel_size %in% rel_sizes_selection), aes(color = "Modell")) +
  geom_point(data = filter(maxis, rel_size %in% rel_sizes_selection) |> mutate(c_name = "WCET"), aes(color = "Modell")) +
  geom_point(data = filter(data_quantiles, rel_size %in% rel_sizes_selection), aes(color = "Selected Data")) +
  geom_point(data = filter(data_maxis, rel_size %in% rel_sizes_selection) |> mutate(c_name = "WCET"), aes(color = "Selected Data")) +
  # scale_x_continuous("Portion of Dataset", labels = scales::label_percent()) +
  scale_y_continuous("WCET [µs]", labels = \(x) x * 1e6) +
  facet_wrap(vars(c_rel_size), ncol = 3, scales = "fixed") +
  labs(
    title = "Jittertest",
    x = "Quantiles and WCET"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 30),
    # axis.title.x = element_blank(),
    axis.title.y = element_text(margin = margin(r = 8)),
    legend.position = "bottom"
  )
save_for_thesis("path-tradeoff-quantiles", prop = 0.74)
#


{
  # for path-fit

  .data <- bind_rows(
    all_quantiles |>
      filter(size == max(size)) |>
      select(c("value", "name")) |>
      mutate(source = "Measurement") |>
      mutate(name = ordered(c(scales::label_percent(accuracy = 1e-3)(name)))),
    quantiles |>
      filter(size == max(size)) |>
      select(c("value", "name")) |>
      mutate(source = "Sampling") |>
      mutate(name = ordered(c(scales::label_percent(accuracy = 1e-3)(name)))),
    all_maxi |>
      filter(size == max(size)) |>
      select(c("value", "name")) |>
      mutate(
        name = "max",
        source = "Measurement"
      ),
    maxis |>
      filter(size == max(size)) |>
      select(, c("value", "name")) |>
      mutate(
        name = "max",
        source = "Sampling"
      )
  )
}
