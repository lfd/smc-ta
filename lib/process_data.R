process_data <- function(filename, start_events, scale_func, csv_skip = 1, save_func = function(...) {}) {

  load_data(filename, start_events, csv_skip)


  ##
  # B) SOJOURN TIMES
  ##

  ##
  # Fit MODEL
  ##
  if (FALSE) {
    data.model <- data.trans |>
      conv_markov_to_fitted_transition() |>
      print()

    data.model.weibull <- data.model$weibull
    # data.model.mixture <- data.model$mixture
    data.model.mixture2 <- data.model$mixture2
    data.model.mixture3 <- data.model$mixture3
    data.model.mixture4 <- data.model$mixture4
    rm(data.model)

    extract_params <- function(data) {
      Q <- data |>
        dplyr::select(event.from, event.to, params) |>
        unnest_longer(params) |>
        acast(event.from ~ event.to ~ params_id, drop = FALSE, value.var = "params")

      return(Q)
    }

    # Q <- select(data.model, event.from, event.to, params) |> unnest_wider(params)
    Q.weibull <- data.model.weibull |>
      extract_params() |>
      print()

    # Q.mixture <- dplyr::select(data.model.mixture, event.from, event.to, params) |>
    #  unnest_longer(params) |>
    #  acast(event.from ~ event.to ~ params_id, drop = FALSE, value.var = "params") |>
    #  print()

    Q.mixture2 <- data.model.mixture2 |>
      extract_params() |>
      print()
    Q.mixture3 <- data.model.mixture3 |>
      extract_params() |>
      print()
    Q.mixture4 <- data.model.mixture4 |>
      extract_params() |>
      print()
  }

  ##
  # Compare sojourn times
  ##

  # data.sojourn <- rbind(
  #  unnest(data.model, fitted_data) |> mutate(type = "expected"),
  #  unnest(data.model, subdata) |> mutate(type = "actual")
  # ) |>
  #  dplyr::select(c(event.from, event.to, duration, type)) |>
  #  dplyr::mutate(transition = paste_trans(event.from, event.to))
  #
  # plots.sojourn <- vis_sojourn(data.sojourn, save_func)


  ##
  # Path (from Fitting)
  ##

  # # TODO: pre-calculate rweibull values
  # .P1 <- P1[is.na(P1)] <- 0
  # which(.P1 != 0, arr.ind = TRUE)
  # data.probs |>
  #   filter(!is.na(probability)) |>
  #   mutate(data = rweibull)
  #
  # fake.durations <- data.model |>
  #   select(c(event.from, event.to, params)) |>
  #   mutate(
  #     data = list(tibble(
  #       duration = rweibull(n_runs, 1 / params[["shape"]], exp(params[["scale"]]))
  #     )),
  #     .keep = "unused"
  #   ) |>
  #   unnest(data) |>
  #   ungroup()

  sample.duration.weibull <- function(Q, prev_state, state) {
    params <- Q[prev_state, state, ]
    duration <- rweibull(1, params["shape"], params["scale"])

    return(duration)
  }

  sample.duration.mixture <- function(Q, prev_state, state) {
    params <- Q[prev_state, state, ]

    component <- sample(1:length(params[["mixing_proportions"]]), size = 1, prob = params[["mixing_proportions"]])
    value <- rnorm(1, mean = params[["mu"]][component], sd = params[["sd"]][component])

    return(value)
  }

  g_path <- function(i,
                     P1,
                     Q,
                     duration_func,
                     data = data.frame(run = numeric(), event.from = factor(), event.to = factor(), duration = numeric())) {
    # print(i) # Debug print
    state <- sample(start_events)
    rows <- list()

    while (TRUE) {
      # Exit run / path / iteration on reaching end state
      if (state %in% end_events) {
        rows[[length(rows) + 1]] <- list(run = i, event.from = state, event.to = NA, duration = NA)
        break
      }

      successor_vector <- P1[state, ]
      prev_state <- state
      state <- sample(names(successor_vector), size = 1, replace = T, prob = successor_vector)

      duration <- duration_func(Q, prev_state, state)

      rows[[length(rows) + 1]] <- list(run = i, event.from = prev_state, event.to = state, duration = duration)
    }

    data <- rbindlist(append(rows, list(data), after = 0))
    # data <- bind_rows(as_tibble(rows), data)
    return(data)
  }

  if (FALSE) {
    .run.mixture <- function(i) {
      g_path(i, P1, Q.mixture, sample.duration.mixture)
    }
    .run.weibull <- function(i) {
      g_path(i, P1, Q.weibull, sample.duration.weibull)
    }


    fake.trans <- furrr::future_map(1:n_runs, .run, .options = furrr_options(seed = TRUE)) |> rbindlist()

    fake.trans |>
      info_transition() |>
      print() -> info.fake.trans


    fake.path <- conv_transition_to_path(fake.trans)

    fake.path |>
      info_path() |>
      print() -> info.fake.path


    compare.trans <- rbind(
      fake.trans |> mutate(type = "sampled"),
      dplyr::select(data.trans, !since.start) |> mutate(type = "measured")
    ) |>
      dplyr::mutate(type = as.factor(type))

    plots.compare.trans <- vis_compare_trans(compare.trans, save_func) |>
      print()

    compare.path <- rbind(
      fake.path |> dplyr::mutate(type = "sampled"),
      data.path |> dplyr::mutate(type = "measured")
    ) |>
      dplyr::mutate(type = as.factor(type))

    plots.compare.path <- vis_compare_path(compare.path, save_func) |>
      print()
  }



  gen.sample_data <- function(df, n, n_runs = 10000, randomized = FALSE) {
    if (randomized) {
      random_sample_values <- sample(unique(df$run), n)
      training_data <- df |>
        dplyr::filter(run %in% random_sample_values)
    } else {
      training_data <- df |>
        dplyr::filter(run <= n)
    }

    print(paste0("Start fitting model for N=", n))

    .data.models <- training_data |>
      conv_markov_to_fitted_transition()

    extract_params <- function(data) {
      Q <- data |>
        dplyr::select(event.from, event.to, params) |>
        unnest_longer(params) |>
        acast(event.from ~ event.to ~ params_id, drop = FALSE, value.var = "params")

      return(Q)
    }

    print("Start extracting model params")

    .Q.weibull <- extract_params(.data.models$weibull)
    .Q.mixture2 <- extract_params(.data.models$mixture2)
    .Q.mixture3 <- extract_params(.data.models$mixture3)
    .Q.mixture4 <- extract_params(.data.models$mixture4)

    run_ids <- 1:n_runs

    print("Start Weibull sampling")
    with_progress({
      p <- progressor(along = run_ids)
      # fake.weibull <- furrr::future_map(1:n_runs, .run.weibull, p = p) |>
      fake.weibull <- furrr::future_map(
        run_ids,
        ~ {
          p()
          g_path(.x, P1, .Q.weibull, sample.duration.weibull)
        },
        .options = furrr::furrr_options(seed = TRUE)
      ) |>
        rbindlist() |>
        dplyr::mutate(method = "Weibull")
    })

    print("Start Mixture (k=2) sampling")
    with_progress({
      p <- progressor(along = run_ids, label = "Mixture2")
      fake.mixture2 <- furrr::future_map(
        run_ids,
        # .run.mixture, p = p
        ~ {
          p()
          g_path(.x, P1, .Q.mixture2, sample.duration.mixture)
        },
        .options = furrr::furrr_options(seed = TRUE, chunk_size = 100)
      ) |>
        rbindlist() |>
        dplyr::mutate(method = "Mixture2")
    })

    print("Start Mixture (k=3) sampling")
    with_progress({
      p <- progressor(along = run_ids, label = "Mixture3")
      fake.mixture3 <- furrr::future_map(
        run_ids,
        # .run.mixture, p = p
        ~ {
          p(message = paste(.x))
          g_path(.x, P1, .Q.mixture3, sample.duration.mixture)
        },
        .options = furrr::furrr_options(seed = TRUE, chunk_size = 100)
      ) |>
        rbindlist() |>
        dplyr::mutate(method = "Mixture3")
    })

    print("Start Mixture (k=4) sampling")
    with_progress({
      p <- progressor(along = run_ids, label = "Mixture4")
      fake.mixture4 <- furrr::future_map(
        run_ids,
        # .run.mixture, p = p
        ~ {
          p()
          g_path(.x, P1, .Q.mixture4, sample.duration.mixture)
        },
        .options = furrr::furrr_options(seed = TRUE, chunk_size = 100)
      ) |>
        rbindlist() |>
        dplyr::mutate(method = "Mixture4")
    })

    print("Start binding data together")
    fake <- rbind(fake.weibull, fake.mixture2, fake.mixture3, fake.mixture4) |>
      mutate(
        N = n,
        transition = paste_trans(event.from, event.to)
      )

    return(fake)
  }

  # Ns <- as.vector(c(10, 25, 50, 100, 200, 400))
  Ns <- 2**(4:13)
  # n_runs <- info.log$run_count
  chunk.trans.fake <- purrr::map(Ns, ~ gen.sample_data(bmm.trans, .x, n_runs = 10000)) |>
    rbindlist()

  with_progress({
    # Ns <- as.vector(c(25, 50, 100, 200, 400, 800, 1600, 3200, 10000)),
    # Ns <- as.vector(c(800, 1600, 3200, 10000))
    # Ns <- as.vector(c(10, 25, 50, 100))
    Ns <- 2**(4:13)
    p <- progressor(along = Ns)
    chunk.trans.fake <- furrr::future_map(
      Ns,
      ~ {
        p()
        gen.sample_data(.x)
      },
      .options = furrr_options(seed = TRUE)
    ) |>
      rbindlist()
  })


  real_data.N <- Inf

  chunk.trans <- rbind(
    chunk.trans.fake |>
      dplyr::mutate(type = "simulated"),
    data.trans |>
      dplyr::mutate(
        N = real_data.N,
        type = "measured",
        method = "Measurement"
      ) |>
      dplyr::mutate() |>
      dplyr::select(!since.start)
  ) |>
    dplyr::mutate(
      # N = as.factor(N), # better to mutate before plots
      type = as.factor(type),
      method = as.factor(method),
      transition = as.factor(transition)
    )

  chunk.path <- chunk.trans |>
    conv_transition_to_path(by = c("N", "type", "method", "run"))


  ##  _  _   __   ____  ____  _  _    ____  __     __  ____  ____  __  __ _   ___
  ## / )( \ / _\ (  _ \(  _ \( \/ )  (  _ \(  )   /  \(_  _)(_  _)(  )(  ( \ / __)
  ## ) __ (/    \ ) __/ ) __/ )  /    ) __// (_/\(  O ) )(    )(   )( /    /( (_ \
  ## \_)(_/\_/\_/(__)  (__)  (__/    (__)  \____/ \__/ (__)  (__) (__)\_)__) \___/

  df.total <- chunk.path
  df.path <- chunk.path
  #df.path <- chunk.path |>
  #  filter(!(method == "gev" & N <=32))
  df.trans <- chunk.trans |>
    filter(!is.na(duration))

  ##
  # TIME SERIES
  ##

  data.path |>
    vis_time_series(by = "path") |>
    # print()
    save_func("time_series-path")

  data.trans |>
    filter(!is.na(event.to)) |>
    vis_time_series(by = "transition") |>
    # print()
    save_func("time_series-transition")


  ##
  # COMPOSITION
  ##

  data.path |>
    vis_composition(composition = "path") |>
    # print()
    save_func("composition-path")

  data.trans |>
    filter(!is.na(event.to)) |>
    vis_composition(composition = "transition") |>
    # print()
    save_func("composition-transition")


  plan(multisession, workers = 8)
  plan(sequential)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(forcats)
  with_progress({
    ll <- list(
      # Mixture2
      #list(comparison = "N", data = filter(df.total, method %in% c("mixture2", "measurement")), by = NULL, name = "total-mixture2"),
      list(comparison = "N", data = filter(df.path, method %in% c("mixture2", "measurement")), by = "path", name = "path-mixture2"),
      #list(comparison = "N", data = filter(df.trans, method %in% c("mixture2", "measurement")), by = "transition", name = "transition-mixture2"),
      ## Mixture3
      #list(comparison = "N", data = filter(df.total, method %in% c("mixture3", "measurement")), by = NULL, name = "total-mixture3"),
      list(comparison = "N", data = filter(df.path, method %in% c("mixture3", "measurement")), by = "path", name = "path-mixture3"),
      #list(comparison = "N", data = filter(df.trans, method %in% c("mixture3", "measurement")), by = "transition", name = "transition-mixture3"),
      ## Mixture4
      #list(comparison = "N", data = filter(df.total, method %in% c("mixture4", "measurement")), by = NULL, name = "total-mixture4"),
      list(comparison = "N", data = filter(df.path, method %in% c("mixture4", "measurement")), by = "path", name = "path-mixture4"),
      #list(comparison = "N", data = filter(df.trans, method %in% c("mixture4", "measurement")), by = "transition", name = "transition-mixture4"),
      ## GEV
      #list(comparison = "N", data = filter(df.total, method %in% c("gev", "measurement")), by = NULL, name = "total-gev"),
      list(comparison = "N", data = filter(df.path, method %in% c("gev", "measurement")), by = "path", name = "path-gev"),
      #list(comparison = "N", data = filter(df.trans, method %in% c("gev", "measurement")), by = "transition", name = "transition-gev"),
      ## Weibull
      #list(comparison = "N", data = filter(df.total, method %in% c("weibull", "measurement")), by = NULL, name = "total-weibull"),
      list(comparison = "N", data = filter(df.path, method %in% c("weibull", "measurement")), by = "path", name = "path-weibull"),
      #list(comparison = "N", data = filter(df.trans, method %in% c("weibull", "measurement")), by = "transition", name = "transition-weibull"),
      ## METHOD
      list(comparison = "method", data = filter(df.path, N == max(N), .by = "method"), by = NULL, name = "total"),
      list(comparison = "method", data = filter(df.path, N == max(N), .by = "method"), by = "path", name = "path"),
      list(comparison = "method", data = filter(df.trans, N == max(N), .by = "method"), by = "transition", name = "transition")
    )

    p <- progressor(steps = length(ll) * (2 * 1 + 2 + 4 * 2), label = "plot")

    #furrr::future_walk(ll, function(.x) {
    purrr::walk(ll, function(.x) {
      ##
      # PROBABILISTIC DURATION
      ##
      sapply(c("ridges", "histogram"), function(.pt) {
        .x$data |>
          vis_x_compare(by = .x$by, plot_type = .pt, comparison = .x$comparison) |>
          # print()
          save_func(paste("compare", .x$comparison, "by", .x$name, .pt, sep = "-"))
        p()
      })

      .x$data |>
        vis_ecdf(by = .x$by, comparison = .x$comparison) |>
        # print()
        save_func(paste("compare", .x$comparison, "by", .x$name, "ecdf", sep = "-"))
      p()

      .x$data |>
        vis_ecdf(by = .x$by, comparison = .x$comparison, with_quantiles = F) |>
        # print()
        save_func(paste("compare", .x$comparison, "by", .x$name, "ecdf-simplified", sep = "-"))
      p()

      ##
      # PROBABILISTIC DURATION: QUANTILES
      ##
      .x$data |>
        vis_quantile(by = .x$by, comparison = .x$comparison) |>
        save_func(paste("compare", .x$comparison, "by", .x$name, "quantile", sep = "-"))
      p()

      .x$data |>
        vis_quantile(by = .x$by, comparison = .x$comparison, many_quantiles = T) |>
        save_func(paste("compare", .x$comparison, "by", .x$name, "many-quantile", sep = "-"))
      p()

      sapply(c("diff", "diff-abs", "diff-rel"), function(.pt) {
        .x$data |>
          vis_quantile(by = .x$by, comparison = .x$comparison, plot_type = .pt) |>
          save_func(paste("compare", .x$comparison, "by", .x$name, "quantile", .pt, sep = "-"))
        p()

        .x$data |>
          vis_quantile(by = .x$by, comparison = .x$comparison, plot_type = .pt, many_quantiles = T) |>
          save_func(paste("compare", .x$comparison, "by", .x$name, "many-quantile", .pt, sep = "-"))
        p()
      })
    }
    #, # options = furrr_options(seed = TRUE)
    )
  })

  #  ____  _                                             _
  # |  _ \| | __ _ _   _  __ _ _ __ ___  _   _ _ __   __| |
  # | |_) | |/ _` | | | |/ _` | '__/ _ \| | | | '_ \ / _` |
  # |  __/| | (_| | |_| | (_| | | | (_) | |_| | | | | (_| |
  # |_|   |_|\__,_|\__, |\__, |_|  \___/ \__,_|_| |_|\__,_|
  #                |___/ |___/


  df <- chunk.path |>
    group_by(N, type, path, length)

  df <- chunk.trans |>
    dplyr::filter(!is.na(event.to)) |>
    group_by(N, type, event.from, event.to, transition)

  result <- data.frame()
  for (block_size in c(25, 50, 100, 200, 400, 800)) {
    df |>
      count() |>
      pluck("n") |>
      max() -> .max_group

    # for (i in seq(from = 1, to = .max_group, by = block_size)) {
    #  chunk.block <- df |>
    #    slice(i:(i + (block_size - 1))) |>
    #    summarise(block_maximum = max(duration), .groups = "keep") |>
    #    mutate(block_size = block_size) |>
    #    rbind(chunk.block)
    # }

    d <- furrr::future_map(
      1:(.max_group / block_size),
      function(i) {
        blocks <- df |>
          slice(((i - 1) * block_size + 1):(i * block_size)) |>
          summarise(block_maximum = max(duration), .groups = "keep") |>
          mutate(block_size = block_size)
        return(blocks)
      }
    ) |> bind_rows()

    if (nrow(result) > 0) {
      result <- rbindlist(list(result, d))
    } else {
      result <- d
    }
  }

  stop()

  chunk.path.block <- result

  chunk.trans.block <- result

  chunk.path.block |>
    ggplot2::ggplot(aes(x = block_maximum, y = as.factor(N))) +
    ggridges::stat_density_ridges(aes(fill = type)) +
    scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    facet_wrap(vars(block_size)) +
    labs(title = "Distribution of block maxima", subtitle = "per block size", x = "Path Duration", y = "Iterations", fill = "Data Origin") +
    theme_ridges()
  save_func("path-duration-blockMax-ridgesByChunk-facetByBlockSize")

  chunk.path.block |>
    ggplot2::ggplot(aes(x = block_maximum, y = as.factor(N))) +
    ggridges::stat_density_ridges(aes(color = as.factor(block_size), fill = as.factor(block_size), linetype = type), alpha = 0.2) +
    scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    labs(title = "Distribution of block maxima", x = "Path Duration", y = "Iterations", color = "Block Size", fill = "Block Size", linetype = "Data Origin") +
    theme_ridges()
  save_func("path-duration-blockMax-ridgesByChunk-overlappingBlockSize")

  chunk.trans.block |>
    dplyr::filter(!is.na(event.to)) |>
    ggplot2::ggplot(aes(x = block_maximum, y = as.factor(N))) +
    ggridges::stat_density_ridges(aes(fill = as.factor(block_size), color = as.factor(block_size), linetype = type), alpha = 0.3) +
    scale_x_continuous(labels = scales::label_number(scale_cut = scales::cut_si("s"))) +
    facet_wrap(vars(transition), scale = "free_x") +
    labs(title = "Distribution of block maxima", subtitle = "per Transition", x = "Transition Duration", y = "Iterations", color = "Block Size", fill = "Block Size", linetype = "Data Origin") +
    theme_ridges()
  save_func("transition-duration-blockMax-ridgesByChunk-overlappingBlockSize")




  library(extRemes)

  r <- fevd(block_maxima, type = "Gumbel")
  summary(r)
  plot(r)

  library(TSclust)

  diss <- unique(chunk.path$N) |>
    map(function(x) {
      type <- "xz"
      diss <- diss.NCD(
        x = filter(chunk.path, N == Inf) |> pluck("duration"),
        y = filter(chunk.path, N == x) |> pluck("duration"),
        type
      )
      data.frame(N = x, diss, type)
    }) |>
    bind_rows()

  .tt <- chunk.path |>
    filter(N == Inf) |>
    summarise(n = n(), min = min(duration), max = max(duration)) |>
    unlist()
  y <- runif(n = .tt[["n"]], min = .tt[["min"]], max = .tt[["max"]])

  base.diss <- diss.NCD(
    x = filter(chunk.path, N == Inf) |> pluck("duration"),
    y,
    type = "xz"
  )

  mutate(diss, diss - base.diss)

  ncd.cmp <- function(data) {
    diss.NCD(
      x = filter(chunk.path, N == Inf) |> pluck("duration"),
      y = filter(chunk.path, N == 400) |> pluck("duration"),
      type = "min"
    )
  }

  # non-parametric test
  # D = max absolute difference between the empirical cumulative distribution functions
  # p = strenght of evidence against the null hypothesis;
  #  smaller pe => stronger evidence against the null hypothesis (i.e. different distributions)

  ks.test(
    x = filter(chunk.path, N == Inf) |> pluck("duration"),
    y = filter(chunk.path, N == 400) |> pluck("duration")
  )

  ks.test(
    x = filter(chunk.path, N == Inf) |> pluck("duration"),
    y
  )

  library(transport)
  wasserstein_dist <- transport::wasserstein1d(observed, predicted)
}
