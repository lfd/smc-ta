fit_weibull <- function(vec, scaling = T, diagnostics = F) {
  # 1. scale data
  # 2. estimate distribution
  # 3. (re)scale estimation params

  if (scaling) {
    scale <- attributes(scale(vec, center = FALSE))[["scaled:scale"]]
    vec <- vec / scale
  }

  model <- fitdistrplus::fitdist(vec, "weibull")

  if (diagnostics) {
    plot(model)
    print(summary(model))
  }

  params <- summary(model)[[1]]
  if (scaling) {
    params[["scale"]] <- params[["scale"]] * scale
  }

  return(params)
}
