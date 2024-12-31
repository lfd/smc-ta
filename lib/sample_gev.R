sample_gev <- function(n = 1, params) {
  extRemes::revd(n, loc = params[["location"]], scale = params[["scale"]], shape = params[["shape"]], type = "GEV")
}
rgev <- sample_gev
