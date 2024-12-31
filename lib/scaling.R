# Returns scaling function
.scale_factory <- function(scale) {
  function(df, ...) {
    if (data.table::is.data.table(df)) {
      df[, duration := duration * scale]
      invisible(df)
    } else {
      dplyr::mutate(df, duration = duration * scale, ...)
    }
  }
}

# Scaling duration values (s -> s)
scale_from_s <- function(df, ...) { df }

# Scaling duration values (ms -> s)
scale_from_ms <- .scale_factory(1e-3)

# Scaling duration values (us -> s)
scale_from_us <- .scale_factory(1e-6)

# Scaling duration values (ns -> s)
scale_from_ns <- .scale_factory(1e-9)

