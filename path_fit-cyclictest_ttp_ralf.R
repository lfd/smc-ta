source("cyclictest-ttp-ralf.R")

context <- unlist(config$context) |> names()
start_events <- config$start_events %||% abort("start_events cannot be NULL")
end_events <- config$end_events # if NULL => derive from data
block_size <- config$block_size %||% 10
N <- config$n_samples %||% 10000
method_name <- config$method %||% abort("method cannot be NULL")

method <- purrr::chuck(h_methods, method_name)

####

data_load_cached("trans.log")
rm(var)

if (!exists("trans.log")) {
  abort("Object not found")
}

{
  if (!exists("mapping")) {
    browser()
    abort("Object missing")
  }
}

trans.src <- trans.log |>
	filter(!is.na(duration)) |>
	filter(cpu_id == 1) |>
  mutate(run = cur_group_id(), .by = all_of(c("run", context))) |>
  select(event.from, event.to, duration, run, {{ context }})

rm(trans.log)

#### HYPER PARAMETER

probs <- h_probs

####

source("_path-fit.R")
