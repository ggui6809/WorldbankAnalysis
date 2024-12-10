#' Impute Missing Values in Wide Data
#'
#' Handles missing values in wide-format data by filtering, reshaping, imputing, and reshaping back.
#'
#' @param data A wide-format data frame with variable-year columns.
#' @return A wide-format data frame with missing values imputed.
#' @export
impute_data <- function(data) {
  data <- data %>%
    select(where(~ mean(is.na(.)) < 0.5))  # Filter columns with less than 50% missing values

  data_long <- reshape_to_long(data)  # Step 1: Reshape to long format
  data_imputed <- impute_long_data(data_long)  # Step 2: Impute missing values
  data_wide <- reshape_to_wide(data_imputed)  # Step 3: Reshape back to wide format

  return(data_wide)
}
