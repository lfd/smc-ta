source("jittertest-ttp.R")

PRINT <- T

data_filename <- config$data_filename %||% abort("data_filename must not be undefined")
scale_func <- match.fun(config$time_scaling) %||% abort("time_scaling cannot be NULL")
start_events <- config$start_events %||% abort("start_events cannot be NULL")
end_events <- config$end_events # if NULL => derive from data
context_types <- unlist(config$context)
context <- names(config$context)

data_load(
  filename = file.path(DATA_DIR, data_filename),
  csv_skip = config$csv_skip,
  start_events = start_events,
  end_events = end_events,
  context = context_types,
  .skip_cache = T
)

#data_load_cached("event.log")
rm(var)

if (!exists("event.log")) {
  abort("Object not found")
}

event.log |>
	select(run, {{ context }}) |>
	unique() |>
	count() |>
	unlist()

####

source("_events-meta.R")
