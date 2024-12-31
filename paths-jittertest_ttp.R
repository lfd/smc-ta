source("jittertest-ttp.R")

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
      path = factor(path, names(mapping), mapping)
    )
}

####

binwidth <- 0.25e-6

source("_paths.R")
