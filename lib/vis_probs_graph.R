vis_probs_graph <- function(P, save_func = function(...) {}, data_type = NULL, width) {
  # save <- function(plot, plot_name, ...) {
  #  save_func(paste0("probability-graph", plot_name))
  # }

  width <- 7
  height <- 4

  if (TRUE) {
    width <- 156 / 25.4 # mm → inch
    height <- 0.75 * width

    path <- OUT_DIR
    f_path <- function(filename) file.path(path, filename)
  }

  graph <- .vis_probs_graph.graph(P)
  if (PRINT) {
    .vis_probs_graph.plot(graph)
  }

  grDevices::png(
    filename = f_path("propability-graph.png"),
    width = 1000, height = 1000, units = "px"
  )
  .vis_probs_graph.plot(graph)
  dev.off()

  tikzDevice::tikz(
    f_path("propability-graph.tikz"),
    width, height,
    sanitize = TRUE,
    # pointsize = 11,
    engine = "pdftex",
    timestamp = T
  )
  .vis_probs_graph.plot(graph)
  dev.off()
  # save(plots[[plot_name]], plot_name)

  invisible(NULL)
}

.vis_probs_graph.plot <- function(graph) {
  plot(
    graph,
    # layout = igraph::layout_as_tree(graph, mode = "out"),
    layout = igraph::layout_in_circle(graph),
    # layout = igraph::layout_with_dh(graph),
    # main = "Directed Acyclic Graph (DAG)",
    edge.label = scales::label_percent()(igraph::E(graph)$weight),
    edge.label.color = igraph::E(graph)$color,
    edge.arrow.size = 0.3,
    edge.curved = 0.3
  )
}

.vis_probs_graph.graph <- function(P) {
  # Prevent issues during plotting via replacing NA -> 0
  P[is.na(P)] <- 0

  P[P < 1e-4 & P != 0] <- 1e-4
  P <- round(P, digits = 4)

  # graph <- igraph::graph.adjacency(P, mode = "directed", weighted = TRUE)
  graph <- igraph::graph_from_adjacency_matrix(P, mode = "directed", weighted = TRUE)

  # Default shape: circle
  #
  # has to be set explicitly;
  # otherwise setting shape for only few nodes will remain other nodes with no shape.)
  igraph::V(graph)$shape <- "circle"


  vertex_colours <- rainbow(vcount(graph)) |> sample()
  V(graph)$color <- vertex_colours
  # Assign the outgoing edges the colour of the source vertex
  edge_colours <- sapply(E(graph), function(e) {
    source_vertex <- ends(graph, e)[1] # Get the source (tail) of the edge
    V(graph)$color[which(V(graph)$name == source_vertex)] # Get the color of the source vertex
  })

  E(graph)$color <- edge_colours


  # Highlight end states
  end_states <- which(igraph::degree(graph, mode = "out") == 0)
  igraph::V(graph)[end_states]$shape <- "square"
  # igraph::V(graph)[end_states]$color <- "red"

  # Highlight start states
  start_states <- which(igraph::degree(graph, mode = "in") == 0)
  # igraph::V(graph)[start_states]$color <- "green"

  return(graph)
}
