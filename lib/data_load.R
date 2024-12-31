data_load <- function(filename, start_events, end_events = NULL, csv_skip = 0L, context = NULL, .skip_cache = F) {
  p <- progressr::progressor(steps = 4L)
  # types <- str_c(unlist(unname(context)), collapse = "")
  types <- context
  context <- names(context)

  # dump_path <- dirname(filename)
  dump_path <- DATA_DIR

  .fast = T

  .load_cached <- function(...) {
    # Do we actually want to use the cached file to load the variable value?
    if (.skip_cache) {
      return(NULL)
    }

    data_load_cached(...)
  }

  if (!PRINT) {
    print <- function(x, ...) {
      invisible(x)
    }
  }

  ##
  # Load data in LOG format
  ##
  if (is_null(.load_cached("event.log"))) {
    inform(sprintf("Read event.log from '%s'", filename))

    event.log <<- readr::read_csv(
      filename,
      skip = csv_skip,
      col_types = c("timestamp" = "n", event = "f", types),
      col_names = c("timestamp", "event", context)
    ) |>
      rename_with(stringr::str_to_lower) %>%
      {
        inform(
          sprintf("Read %s events", nrow(.) |> scales::number(big.mark = ",")),
          body = summarise(., .by = "event", count = n()) |> format()
        )
        .
      } %>%
      as.data.table()
    setorder(event.log, timestamp)
    if (.fast) {
      event.log <<- prepare_event_log_fast.dt(event.log, start_events, end_events, context, with.filtering = F)
    } else {
      event.log <<- prepare_event_log.dt(event.log, start_events, end_events, context)
    }
    event.log <<- event.log[,c("timestamp", "event", "run", "offset", context), with=FALSE]
    dump_var("event.log", dump_path)

    # Depending dataframs should be derived instead of loaded
    .skip_cache <- TRUE
  }

  info.event <<- info_log(event.log) |> print()

  p("event.log loaded")

  # Assertion: given start_events must be an super set of our detected start events
  stopifnot(all(info.event$starting_event_names %in% start_events))
  if (length(end_events) > 0) {
    stopifnot(all(info.event$ending_event_names %in% end_events))
  }

  ##
  # Transform into TRANSITION format
  ##
  if (is_null(.load_cached("trans.log"))) {
    rlang::inform("Derive trans.log from event.log")
    trans.log <<- copy(event.log) |>
      conv_log_to_transition.dt(context) |>
      #conv_log_to_transition(context) |>
      # mutate(transition = paste_trans(event.from, event.to)) |>
      scale_func() |>
      print()

    if (!.fast) {
      # Simplify data
      # CAUTION!!!!
      trans.log <<- trans.log |>
        # filter(n() >= 100 * block_size, .by = transition)
        filter(n() >= 100, .by = transition)
    }

    dump_var("trans.log", dump_path)
    .skip_cache <- TRUE
  }

  # trans.log <<- event.log |>
  #  conv_log_to_transition.dt(context) |>
  #  scale_func() |>

  info.trans <<- info_transition(trans.log) |> print()

  p("trans.log loaded")

  ##
  # 1-step Probabilities
  ##
  if (is_null(.load_cached("trans.probs"))) {
    rlang::inform("Derive trans.probs from trans.log")
    trans.probs <<- trans.log |>
      conv_transition_to_probs.dt() |>
      print()
    dump_var("trans.probs", dump_path)
    .skip_cache <- TRUE
  }

  p("trans.probs loaded")

  ##
  # n-step Probabilities
  ##
  if (is_null(.load_cached("P"))) {
    rlang::inform("Derive trans.probs from trans.log")
    P <<- format_probs_matrix(trans.probs, start_events) |>
      print()
    dump_var("P", dump_path)

    # nothing depending
    # .skip_cache <- TRUE
  }

  p("P loaded")

  ##
  # Derive Paths
  ##
  if (is_null(.load_cached("path.log"))) {
    rlang::inform("Derive path.log from trans.log")
    path.log <<- trans.log |>
      conv_transition_to_path(by = c("run", {{ context }})) |>
      print()
    dump_var("path.log", dump_path)

    .skip_cache <- TRUE
  }

  info.path <<- info_path(path.log) |> print()

  p("path.log loaded")

  ##
  # Misc
  ##
  P1 <<- P[[1]]

  if (rlang::is_empty(end_events)) {
    end_events <<- info.event$ending_event_names
  }
}
