info_path <- function(df, context = NULL) {
  required_columns <- c("path", "length", "duration", context)
  stopifnot(all(required_columns %in% colnames(df)))

  by_context <- NULL

  paths <- df |>
    dplyr::summarise(
      count = n(),
      mean = mean(duration),
      median = median(duration),
      min = min(duration),
      max = max(duration),
      .by = c("path", "length")
    ) |>
    dplyr::mutate(
      # path = as.factor(path),
      frequency = count / sum(count)
    )

  if (!is.null(context)) {
    by <- c("path", "length", context)

    by_context <- df |>
      dplyr::summarise(
        count = n(),
        mean = mean(duration),
        median = median(duration),
        min = min(duration),
        max = max(duration),
        .by = all_of(context)
      ) |>
      dplyr::mutate(
        # path = as.factor(path),
        frequency = count / sum(count),
        .by = all_of(context)
      )
  }

  list(
    path_names = paths$path,
    paths = paths,
    by_context = by_context
  )
}
