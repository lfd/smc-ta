source("cyclictest-ttp-ralf.R")

start_events <- config$start_events %||% abort("start_events cannot be NULL")
end_events <- config$end_events # if NULL => derive from data
context_types <- unlist(config$context)
context <- names(config$context)

data_load_cached("event.log")
rm(var)

if (!exists("event.log")) {
  abort("Object not found")
}

event.log |>
	select(run, {{ context }}) |>
	unique() |>
	count() |> unlist()

####

source("_events-meta.R")
