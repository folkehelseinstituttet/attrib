lag_data <- function(x, lags, ref, by) {
  lag_0 <- NULL
  x <- as.vector(x)
  data <- data.table::data.table(lag_0 = x, by = by)

  for (i in 1:lags) {
    data[, glue::glue("lag_{i}") := shift(lag_0, n = i, fill = ref), by = by]
  }

  data[, by := NULL]
  data <- as.matrix(data)
  colnames(data) <- c(1, 2, 3)
  return(data)
}
