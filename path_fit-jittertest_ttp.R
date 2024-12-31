source("jittertest-ttp.R")

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

trans.src <- trans.log |>
	filter(!is.na(duration)) |>
  mutate(run = cur_group_id(), .by = all_of(c("run", context))) |>
  select(event.from, event.to, duration, run, {{ context }})

rm(trans.log)

#### HYPER PARAMETER

probs <- h_probs

unique(path.log$path)
browser() # Edit mapping manually

mapping <- list(
  "expected→6→4→real" = 1,
  "expected→real" = 2
)

####

source("_path-fit.R")
