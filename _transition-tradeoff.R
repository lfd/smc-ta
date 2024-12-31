if (!exists("trans.src")) {
  abort("Object not found")
}

if (!exists("many_transitions")) {
  many_transitions <- length(unique(trans.src$transition)) >= 9
}

require(ggplot2)

{
  trans_bm <- function(block_size) {
    if (block_size < 2) {
      return(trans.src)
    }

    pick_block_maxima(trans.src, block_size, by = c("transition", context)) |>
      mutate(block_size = block_size)
  }


  plot <- ggplot(mapping = aes(x = duration)) +
    facet_wrap(c("transition"), scales = "free_x", ncol = ifelse(many_transitions, 3, 2))

  block_sizes <- c(1, seq(5, 30, 5), 50, 100)
  block_sizes |>
    purrr::walk(function(block_size) {
      message(block_size)
      plot <<- plot +
        stat_ecdf(data = trans_bm(block_size), aes(color = block_size, group = transition))
    })
  plot <- plot +
    scale_color_gradient2(low = "black", mid = scales::muted("blue"), high = "red", midpoint = median(block_sizes)) +
    scale_x_continuous("Transition Duration [µs]", labels = \(x) x * 1e6) +
    scale_y_continuous("ECDF") +
    labs(color = "Block Size")
  save_for_thesis("transition-ecdf-block_sizes", plot = plot, full_page = many_transitions)
}


{
  block_size <- config$block_size %||% 10

  levels <- c("Discarded", "Block Maximum") # set order for ggplot's legend

  trans.src |>
    mutate(transition = paste_trans(event.from, event.to)) |>
    pick_block_maxima(block_size, by = c("transition", context), dry = T) |>
    mutate(filtered = if_else(filtered, "Discarded", "Block Maximum") |> factor(levels = levels)) |>
    ggplot(aes(x = duration, y = after_stat(count))) +
    scale_x_continuous("Transition Duration [µs]", labels = function(x) x * 1e6) +
    stat_bin(aes(fill = filtered), binwidth = 0.25e-6, geom = "bar", position = "stack") +
    # stat_bin(bins = 100, geom = "bar", position = "stack") +
    # scale_x_continuous("Transition Duration", labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    scale_fill_manual("", values = c("gray50", "blue")) +
    facet_wrap(c("transition"), scales = "free", ncol = 2) +
    labs(y = "Count") +
    theme_minimal()
  save_for_thesis("transition-filter-histogram", prop = 0.6)
  save_for_thesis("transition-filter-histogram", full_page = T)

  rm(block_size)
}
