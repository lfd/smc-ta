fit_gev <- function(vec, scaling = T, diagnostics = F, shape = 0.1) {
  if (F && scaling) {
    scale <- attributes(scale(vec, center = FALSE))[["scaled:scale"]]
    vec <- vec / scale
  }

  model <- extRemes::fevd(vec, type = "GEV", method = "MLE", initial = list(location = mean(vec), scale = sd(vec), shape = shape))

  params <- model[["results"]][["par"]]
  params <- split(unname(params),names(params))

  return(params)
}
