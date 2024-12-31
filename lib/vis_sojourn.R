#' Visualize sojourn times
#'
#' This function generates plots visualizing sojourn times.
#'
#' @param data A data frame containing the necessary variables for plotting.
#' @return NULL
#'
vis_sojourn <- function(data, save_func = function(...) {}, data_type = NULL) {
  save <- function(plot_name, ...) {
    save_func(paste0(data_type, "sojourn-", plot_name))
  }

  plots <- list()

  plot_name <- "distributions"
  plots[[plot_name]] <- .sojourn_rig_density(data)
  save(plot_name)

  plot_name <- "distributions-log"
  plots[[plot_name]] <- .sojourn_rig_density_log10(data)
  save(plot_name)

  return(plots)
}

#' Generate a density ridges plot for sojourn times
#'
#' This function generates a density ridges plot specifically for sojourn times.
#'
#' @param data A data frame containing the necessary variables for plotting.
#' @return A ggplot2 object representing the density ridges plot.
#' @import ggplot2
#' @import scales
#' @import ggridges
#'
#' @keywords internal
.sojourn_rig_density <- function(data) {
  plot <-
    ggplot(data, aes(
      x = duration,
      color = type
    )) +
    stat_density_ridges(
      aes(
        y = type,
        fill = factor(after_stat(quantile))
      ),
      geom = "density_ridges_gradient",
      calc_ecdf = TRUE,
      quantiles = c(0.025, 0.975)
      # fill = NA
    ) +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(
      labels = scales::label_number(scale_cut = scales::cut_si("s")),
      expand = c(0, 0),
      limits = c(0, NA)
    ) +
    scale_fill_manual(
      name = "Probability",
      values = c("#FF0000A0", "#A0A0A0A0", "#0000FFA0"),
      labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]")
    ) +
    coord_cartesian(clip = "off") +
    theme(axis.text.y = element_blank()) +
    facet_wrap(~transition, scales = "free_x") +
    labs(color = NULL, x = "Sojourn Time", y = "Density [a.u.]")

  return(plot)
}

#' Generate a logarithmic density ridges plot for sojourn times
#'
#' This function generates a density ridges plot specifically for sojourn times.
#'
#' @param data A data frame containing the necessary variables for plotting.
#' @return A ggplot2 object representing the density ridges plot.
#' @import scales
#'
#' @keywords internal
.sojourn_rig_density_log10 <- function(data) {
  plot <- .sojourn_rig_density(data) +
    scale_x_log10(
      labels = scales::label_number(scale_cut = scales::cut_si("s")),
      expand = c(0, 0)
    )

  return(plot)
}
