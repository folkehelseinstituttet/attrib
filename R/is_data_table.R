# Check is data is a data table

# @param data Data to be checked
is_data_table <- function(data) {
  if (is.data.table(data) == FALSE) {
    stop("The dataset is not a data table")
  }
}
