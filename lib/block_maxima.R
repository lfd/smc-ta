block_maxima <- function(x, block_size = 100) {
	stopifnot(is.numeric(x) && is.vector(x) && !is.matrix(x) && !is.list(x))
	stopifnot(block_size > 0) # && is.wholenumber(block_size))

  .block_seq <- function(i) ((i - 1) * block_size + 1):(i * block_size)
  .block_maximum <- function(i) max(x[.block_seq(i)])

  n_blocks <- length(x) / block_size
  block_maxima <- sapply(1:n_blocks, .block_maximum)

  return(block_maxima)
}
