source("cyclictest-ttp-ralf.R")

context <- unlist(config$context) |> names()
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
  filter(cpu_id == 1) |>
  mutate(run = cur_group_id(), .by = all_of(c("run", context))) |>
  select(event.from, event.to, duration, {{ context }})

rm(trans.log)
####

fit_fun <- purrr::chuck(method, "fit_fun")
probs <- c(.9, .99, .999, .9999, .99999)

##################


{
  plot <- ggplot()

  Qs <- tibble()

  d_maxi <- tibble()
  d_quantiles <- tibble()
  options(digits = 7)
  probs <- c(.9, .99, .999, .9999, .99999)

  block_sizes <- c(1, seq(5, 30, 5), 50, 100)
  for (bs in block_sizes) {
    df <- trans_bm(bs)
    Q <- df |>
      method_fit(fit_fun) |>
      mutate(transition = paste_trans(event.from, event.to))

    for (i in seq(1, nrow(Q))) {
      params <- Q[i, ]$params |> unlist(recursive = F)
      if (any(is.na(unlist(params)))) {
        next
      }

      data <- tibble(
        duration = sample_mixture(10000, params),
        transition = Q[i, ]$transition
      )
      quantiles <- data |>
        calc_quantiles(by = c("transition"), probs = probs) |>
        mutate(
          name = as.factor(name),
          block_size = factor(bs, levels = block_sizes)
        )
      maxi <- data |>
        summarise(duration = max(duration), .by = c("transition")) |>
        mutate(
          block_size = factor(bs, levels = block_sizes)
        )
      Qs <- bind_rows(Qs, Q)
      d_quantiles <- bind_rows(d_quantiles, quantiles)
      d_maxi <- bind_rows(d_maxi, maxi)

      # plot <- plot +
      #  geom_point(data = quantiles, aes(x = name, y = value, color = block_size, shape = block_size)) +
      # 	geom_hline(data = maxi, aes(yintercept = duration, color = block_size))
    }
  }
  # plot <- plot +
  #  # stat_density(data = df, aes(x = duration), geom = "line", color = "red")+
  #  facet_wrap(vars(transition), scales = "free", ncol = 3)
  ## scale_x_continuous("Transition Duration [µs]", labels = \(x) x * 1e6)
  ## print(plot)

  d_quantiles |>
    mutate(
      block_size = block_sizes[block_size],
      name = probs[name]
    ) |>
    ggplot(aes(x = factor(name), y = value, color = block_size)) +
    geom_point(
      # aes(shape = block_size),
      show.legend = F
    ) +
    geom_line(aes(group = block_size)) +
    facet_wrap(vars(transition), scales = "free", ncol = 3) +
    scale_x_discrete("Quantile") +
    # scale_x_continuous("Quantile", transform = "log10", labels = \(x) paste0("1-",scales::label_scientific()(1-x))) +
    scale_y_continuous("Quantile Value") +
    scale_color_gradientn(colors = c("blue", "cyan", "green", "red", "black")) +
    # scale_color_gradient2(low = "black", mid = scales::muted("blue"), high = "red", midpoint = median(block_sizes)) +
    labs()
  save_for_thesis("transition-quantiles-block_sizes", full_page = T)

  d_maxi |>
    mutate(
      block_size = block_sizes[block_size],
    ) |>
    ggplot(aes(x = block_size, y = duration)) +
    geom_point( show.legend = F ) +
    geom_line()+
    facet_wrap(vars(transition), scales = "free", ncol = 3) +
    scale_x_continuous("Block Size") +
    scale_y_continuous("WCET")
    save_for_thesis("transition-maxi-block_sizes", full_page = F)
}


BROKEN
{
  p <- ggplot()

  add_stat <- function(mean, sd, transition, prop, id) {
    message(mean)
    message(sd)
    colors <- c("red", "blue", "green", "black")
    p <<- p +
      geom_vline(color = colors[[id]], xintercept = as.numeric(mean)) +
      stat_function(
        data = tibble(
          transition = Q[i, ]$transition,
          id = factor(j, levels = as.character(1:4)),
          mean = as.numeric(mean)
        ),
        color = colors[[id]],
        fun = function(x) {
          (dnorm(x, mean = mean, sd = as.numeric(sd) * 100)) # * as.numeric(prop)
        }
      )
  }

  for (i in seq(1, 1)) {
    params <- Q[i, ][["params"]] |> unlist(recursive = F)
    # for (j in seq(1, length(params$mu))) {
    for (j in seq(1, 4)) {
      message(paste("j =", j))
      add_stat(
        mean = params$mu[[j]],
        sd = params$sd[[j]],
        prop = params$mixing_proportions[[j]],
        id = j
      )
    }
    rm(j)
  }
  rm(i)
  p <<- p + xlim(4e-6, 20e-6)
  p
}

BROKEN
{
  ggplot() +
    mapply(
      function(mean, sd, prop) {
        stat_function(
          fun = function(x) {
            (dnorm(x, mean = mean, sd = sd))
          }
        )
      },
      mean = mix[["mu"]],
      sd = mix[["sd"]],
      prop = mix[["mixing_proportions"]]
    ) +
    xlim(0, 100e-6)
}
