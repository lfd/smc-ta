pick_block_maxima <- function(df, block_size, values_from = "duration", by = NULL, dry = F) {
  df <- df |>
    filter(!is.na(duration)) |>
    ## This is the old implementation
		# reframe(duration = compute_block_maxima(duration, block_size), .by = all_of(by)) |>
    # mutate(block = row_number(), run = block, .by = all_of(by))

    ## This is the new implementation
		mutate(block_id = (row_number() - 1) %/% block_size, .by = all_of(by))

  if(!dry){
	  df <- df |>
			filter(duration == max(duration), .by = all_of(c("block_id", by)))
  } else {
  	df <- df |>
  		mutate(filtered = (duration != max(duration)), .by = all_of(c("block_id", by)))
  }

  df
}
