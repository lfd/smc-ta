derive_P <- function(dt) {
  required_columns <- c("event.from", "event.to")
  stopifnot(all(required_columns %in% colnames(dt)))

  probs <- dt[, .(n = .N), by = .(event.from, event.to)]
  probs[, probability := n / sum(n), by = event.from]
  probs <- probs[, !"n"]
  setorder(probs, event.from, event.to)

  return(probs)
}

format_probs_matrix <- function(dt, start_events, max_iter = 100) {
  required_columns <- c("event.from", "event.to", "probability")
  stopifnot(all(required_columns %in% colnames(dt)))

  probs <- dcast(dt[!is.na(event.to)], event.from ~ event.to, drop = FALSE, value.var = "probability")

  # data.tables have issues with row names?
  probs <- as.data.frame(probs)
  rownames(probs) <- probs$event.from
  probs$event.from <- NULL
  probs <- as.matrix(probs)

  stopifnot(is.matrix(probs))

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
  #diag(probs) <- NA

  P <- list(probs.calc)
  i <- 1

  for(i in 1:max_iter) {
  	p_i <- P[[i]]
    if (all(is.na(p_i) | p_i == 0)) {
      break
    }

    P[[i + 1]] <- p_i %*% probs.zeroed
  }

  return(P[-(length(P))]) # Remove last matrix which contains only zero probabilities
}
