source("cyclictest-ttp-ralf.R")

context <- unlist(config$context) |> names()

####

data_load_cached("path.log")
rm(var)

if (!exists("path.log")) {
  abort("Object not found")
}

####

{
  if (!exists("mapping")) {
    path.log |>
      summarise(n = n(), .by = path) |>
      arrange(desc(n)) |>
      mutate(
        freq = n / sum(n),
        c_freq = scales::label_percent()(freq)
      )
    browser()
    abort("Object missing")
  }

  stopifnot(all(unique(path.log$path) %in% names(mapping)))

  path.src <- path.log |>
    mutate(
      path_name = path,
      path = ordered(path, names(mapping), mapping)
    )

  path.src <- path.src |>
    filter(cpu_id == 1)
}


rm(path.log)

####

source("_paths.R")
