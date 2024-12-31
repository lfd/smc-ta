source("cyclictest-ttp-ralf.R")

PRINT <<- config$print

start_events <- config$start_events %||% abort("start_events cannot be NULL")
end_events <- config$end_events # if NULL => derive from data
context_types <- unlist(config$context)
context <- names(config$context)
block_size <- config$block_size %||% 10
N <- config$n_samples %||% 10000
method <- config$method

####

data_load_cached("trans.log")

if (!exists("trans.log")) {
  abort("Object not found")
}

trans.src <- trans.log |>
	filter(cpu_id == 1) |>
  mutate(run = cur_group_id(), .by = all_of(c("run", context)))

rm(trans.log, var)

#### HYPER PARAMETER

#h_sizes <- floor((max(trans.src$run) * c(seq.int(1,9), seq.int(10, 90, 10), seq.int(100, 900, 100), 1000)) %/% 1000) |> rev()
#h_sizes <- floor(max(trans.src$run) * rel_sizes_selection) |> rev()
h_sizes <- floor((max(trans.src$run) * seq.int(1, 10)) %/% 10) |> rev()

#h_block_sizes <- c(block_size)
h_block_sizes <- c(1, seq(5, 30, 5), 50, 100) |> rev()

probs <- c(.9, .99, .999, .9999, .99999)

####


#combinations <- expand.grid(size = h_sizes, method_name = method, block_size = c(block_size), N = N)
combinations <- expand.grid(size = h_sizes, method_name = names(h_methods)[2:4], block_size = h_block_sizes, N = N)
combinations["method"] <- list(unname(h_methods[as.character(combinations$method_name)]))

####

options(future.globals.maxSize = 1000000000)

source("_path-fit.R")
