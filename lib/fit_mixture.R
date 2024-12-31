fit_mixture <- function(vec, k, scaling = T, diagnostics = F) {
  stopifnot(!(is.na(k) || is.null(k))) # or default to:  k <- 2 * floor(log10(length(vec)))

  if (scaling) {
    scale <- attributes(scale(vec, center = FALSE))[["scaled:scale"]]
    vec <- vec / scale
  }

  params <- tryCatch(
    expr = {
      # Estimate the parameters of a mixture of Gaussian distributions
      # https://rdrr.io/cran/nor1mix/man/norMixFit.html
      estimation <- nor1mix::norMixEM(vec, k)

      # Returning a list of parameters
      # https://rdrr.io/cran/nor1mix/man/norMix.html
      params <- list(
        mu = estimation$mu,
        sd = sqrt(estimation$sigma^2), # sigma^2 are the variances
        mixing_proportions = estimation$w
      )
      return(params)

      # Estimate the parameters of a mixture of Gaussian distributions
      estimation <- mixtools::normalmixEM(vec, k = k, maxrestarts=2000)

      # Returning a list of parameters
      list(
        mu = estimation$mu,
        sd = sqrt(estimation$sigma^2), # sigma^2 are the variances
        mixing_proportions = estimation$lambda
      )
    },
    error = function(e) {
      message("Unable to fit mixture with k=", k, " on ", length(vec), " events")
      print(e)

      list(
        mu = rep(NA, k),
        sd = rep(NA, k),
        mixing_proportions = rep(NA, k)
      )
    }
  )

  if (scaling) {
    params[["mu"]] <- params[["mu"]] * scale
    params[["sd"]] <- params[["sd"]] * scale
  }

  return(params)
}

fit_mixture2 <- function(...) fit_mixture(k = 2, ...)
fit_mixture3 <- function(...) fit_mixture(k = 3, ...)
fit_mixture4 <- function(...) fit_mixture(k = 4, ...)
