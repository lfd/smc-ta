source("jittertest-ttp.R")

PRINT <<- config$print

start_events <- config$start_events %||% abort("start_events cannot be NULL")
end_events <- config$end_events # if NULL => derive from data
context_types <- unlist(config$context)
context <- names(config$context)
block_size <- config$block_size %||% 10
N <- config$n_samples %||% 10000

####

data_load_cached("trans.log")

if (!exists("trans.log")) {
  abort("Object not found")
}

trans.src <- trans.log |>
  mutate(run = cur_group_id(), .by = all_of(c("run", context)))

rm(trans.log, var)

#### HYPER PARAMETER

h_sizes <- floor(c(max(trans.src$run) * c(seq.int(1, 9), seq.int(10, 90, 10), 100)) %/% 100) |> rev()

h_block_sizes <- config$block_size
h_block_sizes <- c(1, seq(5, 30, 5), 50, 100) |> rev()

probs <- c(.9, .99, .999, .9999, .99999)

####

method_names <- config$method
method_names <- h_methods[names(h_methods) |> startsWith("nmixture")] |> names()

combinations <- expand.grid(size = h_sizes, method_name = method_names, block_size = h_block_sizes, N = N)
combinations["method"] <- list(unname(h_methods[as.character(combinations$method_name)]))

#rm(h_methods, h_sizes, h_block_sizes)

####

source("_tradeoff.R")

combinations <- expand.grid(size = c(8969), method_name = method_names[2], block_size = c(25), N = N)
combinations["method"] <- list(unname(h_methods[as.character(combinations$method_name)]))
