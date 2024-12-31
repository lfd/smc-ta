conv_P_to_simple <- function(P) {
  # Identify rows meeting the condition: all NA
  rows_all_na <- apply(P, 1, function(row) all(is.na(row)))

  # Identify columns meeting the condition: all NA or 0
  columns_na_or_0 <- apply(P, 2, function(col) all(is.na(col) | col == 0))

  # Combined condition: remove entries satisfying both
  to_remove <- rows_all_na & columns_na_or_0
  P[!to_remove, !to_remove]
}
