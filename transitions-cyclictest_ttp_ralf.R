source("cyclictest-ttp-ralf.R")

context <- unlist(config$context) |> names()

####

data_load_cached("trans.log")
rm(var)

if (!exists("trans.log")) {
  abort("Object not found")
}

trans.src <- trans.log |>
  filter(cpu_id == 1) |>
  filter(!is.na(duration)) |>
  select(event.from, event.to, duration, {{ context }}) |>
  mutate(transition = as.factor(paste_trans(event.from, event.to)), .keep = "unused")

rm(trans.log)

source("_transitions.R")
