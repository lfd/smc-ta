format_probs_matrix <- function(df, start_events, max_iter = 100) {
  required_columns <- c("event.from", "event.to", "probability")
  stopifnot(all(required_columns %in% colnames(df)))

  probs <- df |>
    dplyr::filter(!is.na(event.to)) |>
    acast(event.from ~ event.to, drop = FALSE, value.var = "probability")

  end_states <- apply(is.na(probs), 1, all)
  end_events <- end_states |>
    which(arr.ind = TRUE) |>
    names()

  # Prepare for matrix multiplication
  probs[is.na(probs)] <- 0
  probs[end_states, ] <- NA
  probs.calc <- probs
  probs.zeroed <- probs
  probs.zeroed[is.na(probs.zeroed)] <- 0

  probs


  P <- list(probs.calc)
  i <- 1

  #diag(probs) <- NA

  while (TRUE) {
    # Debug print
    # run_complete <- P[[i]][start_events, end_events] != 0
    # if (any(run_complete)) {
    #  print(paste0("1 --", i, "--> ", paste(names(run_complete)[which(run_complete, arr.ind = TRUE)], sep = ",")))
    # }

    if (all(coalesce(P[[i]], 0) == 0) || i > max_iter) {
      break
    }

    P[[i + 1]] <- P[[i]] %*% probs.zeroed
    i <- i + 1
  }

  return(P[-(length(P))]) # Remove last matrix which contains only zero probabilities
}
