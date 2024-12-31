if (!exists("trans.src")) {
  abort("Object not found")
}

if (!exists("many_transitions")) {
  many_transitions <- length(unique(trans.src$transition)) >= 9
}

####

{
  binwidth <- ifelse(exists("binwidth"), binwidth, 0.1e-6)
  trans.src |>
    ggplot(aes(duration)) +
    stat_bin(aes(fill = transition), binwidth = binwidth, show.legend = F) +
    stat_bin(geom = "line", binwidth = binwidth) +
    scale_x_continuous("Transition Duration [µs]", labels = \(x) x * 1e6) +
    scale_fill_viridis_d() +
    labs(y = "Count") +
    facet_wrap(
      vars(transition),
      scales = "free",
      ncol = 2
    ) + theme_minimal()
  save_for_thesis("transition-histogram", prop = 0.5)
  save_for_thesis("transition-histogram", full_page = T)
}

if (F) {
  trans.src |>
    ggplot(aes(duration)) +
    stat_ecdf(aes(color = transition)) +
    scale_x_continuous("Transition Duration [µs]", labels = \(x) x * 1e6) +
    labs(y = "ECDF")
}

####

data_load_cached("P")

if (!exists("P")) {
  abort("Object not found")
}

{
  P.s <- P |>
    purrr::map(conv_P_to_simple)

  P.s |>
    vis_npath(
      save_func = function(...)save_for_thesis(full_page = T, ...),
      selection = c(1,2,3),
    ) |>
    purrr::walk(print)

  #P.s |>
  #  vis_adjacency_matrix() |>
  #  purrr::walk(print)
}
