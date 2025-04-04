
  TIME_SCALES <- list(
    ms = \(x) x * 1e-3,
    us = \(x) x * 1e-6,
    ns = \(x) x * 1e-9,
    ps = \(x) x * 1e-12
  )
  FIT_FUNS <- list(
    mixture2 = \(vec) fit_mixture(vec, 2),
    mixture3 = \(vec) fit_mixture(vec, 3),
    mixture4 = \(vec) fit_mixture(vec, 4)
  )
  METHODS <- list(
    mixture2 = list(
      fit_fun = \(vec) fit_mixture(vec, 2),
      sample_fun = \(n = 1, params) nor1mix::rnorMix(n, params),
      d_generator = \(fit) function(x) {
        fit[1, "w"] * dnorm(x, mean = fit[1, "mu"], sd = fit[1, "sigma"]) +
          fit[2, "w"] * dnorm(x, mean = fit[2, "mu"], sd = fit[2, "sigma"])
      }
    ),
    mixture3 = list(
      fit_fun = \(vec) fit_mixture(vec, 3),
      sample_fun = \(n = 1, params) nor1mix::rnorMix(n, params),
      d_generator = \(fit) function(x) {
        fit[1, "w"] * dnorm(x, mean = fit[1, "mu"], sd = fit[1, "sigma"]) +
          fit[2, "w"] * dnorm(x, mean = fit[2, "mu"], sd = fit[2, "sigma"]) +
          fit[3, "w"] * dnorm(x, mean = fit[3, "mu"], sd = fit[3, "sigma"])
      }
    ),
    mixture4 = list(
      fit_fun = \(vec) fit_mixture(vec, 4),
      sample_fun = \(n = 1, params) nor1mix::rnorMix(n, params),
      d_generator = \(fit) function(x) {
        fit[1, "w"] * dnorm(x, mean = fit[1, "mu"], sd = fit[1, "sigma"]) +
          fit[2, "w"] * dnorm(x, mean = fit[2, "mu"], sd = fit[2, "sigma"]) +
          fit[3, "w"] * dnorm(x, mean = fit[3, "mu"], sd = fit[3, "sigma"]) +
          fit[4, "w"] * dnorm(x, mean = fit[4, "mu"], sd = fit[4, "sigma"])
      }
    )
  )
