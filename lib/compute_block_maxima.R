compute_block_maxima <- function(durations, block_size) {
  n_blocks <- ceiling(length(durations) / block_size)
  block_maxima <- sapply(1:n_blocks, function(i) {
    start <- (i - 1) * block_size + 1
    end <- min(i * block_size, length(durations))
    max(durations[start:end])
  })
  return(block_maxima)
}
