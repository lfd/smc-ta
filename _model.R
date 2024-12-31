if (!exists("P")) {
  abort("Object not found")
}

P1 <- P[[1]]
P1

vis_probs_graph(P1)

{
  P1.simple <- conv_P_to_simple(P1)

  vis_probs_graph(P1.simple, save_for_thesis)
}

