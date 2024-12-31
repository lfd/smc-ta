sample_weibull <- function(n = 1, params) {
  rweibull(n, params[["shape"]], params[["scale"]])
}
