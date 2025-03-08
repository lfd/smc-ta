fit_mixture <- function(vec, k, scaling = T, diagnostics = F, ...) {
  k <- min(k, length(unique(vec)))
  model <- NULL
  i <- 0
  while (is.null(model) && i < 10) {
    tryCatch(
      expr = {
        m <- kmeans(vec, k)$cluster
        if (k < 2) {
          return(NULL)
        }
        if (any(is.na(m))) {
          m <- k
        }
        model <- nor1mix::norMixEM(vec, m, trace = 3)
      },
      error = function(e) print(e),
      finally = {
        i <- i + 1
      }
    )
  }
  return(model)
}

fit_mixture2 <- function(vec, ...) fit_mixture(vec, k = 2, ...)
fit_mixture3 <- function(vec, ...) fit_mixture(vec, k = 3, ...)
fit_mixture4 <- function(vec, ...) fit_mixture(vec, k = 4, ...)
