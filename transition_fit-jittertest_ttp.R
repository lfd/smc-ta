source("jittertest-ttp.R")

context <- unlist(config$context) |> names()
block_size <- config$block_size %||% 10
# N <- config$n_samples %||% 10000
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
  select(event.from, event.to, duration, {{ context }})

rm(trans.log)

####

fit_fun <- purrr::chuck(method, "fit_fun")

probs <- c(.9, .99, .999, .9999, .99999)

####

binwidth <- 0.25e-6

source("_transition-fit.R")
