fit_nmixture <- function(vec, k, scaling = T, diagnostics = F, ...) {
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

fit_nmixture2 <- function(vec, ...) fit_nmixture(vec, k = 2, ...)
fit_nmixture3 <- function(vec, ...) fit_nmixture(vec, k = 3, ...)
fit_nmixture4 <- function(vec, ...) fit_nmixture(vec, k = 4, ...)
