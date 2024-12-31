vis_npath <- function(P, save_func = function(...) {}, selection = NULL, data_type = NULL) {
  save <- function(plot_name, ...) {
    save_func(filename = paste0(data_type, "probability-", plot_name))
  }

  plots <- list()

  for (i in 1:length(P)) {
    # diag(P[[i]]) <- NA
    P[[i]] <- round(P[[i]], digits = 4)
  }

  p_label <- \(x) scales::label_percent(acccuracy = 0.1)(round(x, 4))
  {
    P.df <- reshape2::melt(P) |>
      dplyr::mutate(
        to = Var2,
        from = Var1,
        length = L1,
        probability = value,
        .keep = "unused"
      )
    if (!is.null(selection)) {
      P.df <- P.df |>
        filter(length %in% selection)
    }
    P.df <- P.df |>
      dplyr::mutate(length = paste(length, "steps")) |>
      dplyr::mutate(probability = if_else(to == from, NA, probability))
  }


  if (F) {
    plot_name <- "nfold"
    plots[[plot_name]] <- P.df |>
      ggplot(aes(x = factor(to), y = factor(from), fill = probability)) +
      geom_tile(color = "black") +
      geom_text(aes(label = p_label(probability)), na.rm = TRUE) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      scale_fill_gradient("Transition Probability", low = "grey95", high = "orangered", na.value = "#000000ff", labels = p_label) +
      labs(title = "n-step Transition Probabilities", x = "State To", y = "State From") +
      ggh4x::facet_wrap2(vars(length), scales = "fixed", axes = "all") +
      guides(fill = "none") +
      theme_minimal()
    save(plot_name)
  }

  plot_name <- "nfold_alt"
  .lengths <- unique(P.df$length)
  plots[[plot_name]] <-
    lapply(.lengths, function(l) {
      P.df |>
        filter(length == l) |>
        ggplot(aes(x = factor(to), y = factor(from), fill = probability)) +
        geom_tile(color = "black") +
        geom_text(aes(label = p_label(probability)), na.rm = TRUE) +
        scale_x_discrete(guide = guide_axis(angle = 90)) +
        scale_fill_gradient("Transition Probability", low = "grey95", high = "orangered", na.value = "#000000ff", labels = p_label) +
        labs(x = "State To", y = "State From") +
        guides(fill = "none") +
        theme_minimal()
    }) %>% cowplot::plot_grid(plotlist = ., labels = .lengths, scale = 0.9, vjust = 1.1, hjust = 0, ncol = 1)
  save(plot_name)

  return(plots)
}

vis_adjacency_matrix <- function(P, save_func = function(...) {}, i = NULL, data_type = NULL) {
  save <- function(plot_name, ...) {
    save_func(filename = paste0(data_type, "probability-", plot_name))
  }

  if (is.null(i)) {
    i <- seq_along(P)
  }

  plots <- lapply(c(i), function(i) {
    p <- P[[i]]

    diag(p) <- NA

    plot_name <- paste0(i, "fold")
    p_label <- scales::label_percent()
    plot <- reshape2::melt(p) |>
      dplyr::mutate(
        to = Var2,
        from = Var1,
        # length = L1,
        probability = value,
        .keep = "unused"
      ) |>
      # dplyr::mutate(length = paste(length, "steps")) |>
      ggplot(aes(x = factor(to), y = factor(from), fill = probability)) +
      geom_tile(color = "black") +
      geom_text(aes(label = p_label(probability)), size = 2, na.rm = TRUE) +
      scale_x_discrete(guide = guide_axis(angle = 90)) +
      scale_fill_gradient("Transition Probability", low = "grey95", high = "orangered", na.value = "#000000ff", labels = p_label) +
      labs(title = paste0(i, "-step Transition Probabilities"), x = "State To", y = "State From") +
      theme_minimal()
    save(plot_name)
    return(plot)
  })
}
