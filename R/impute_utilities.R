#' Reshape Data to Long Format
#'
#' Converts wide-format data to long-format, separating variable and year from column names.
#'
#' @param data A data frame in wide format with columns containing variable-year values.
#' @return A data frame in long format with columns: `country`, `countryiso3code`, `variable`, `year`, and `value`.
#' @export
reshape_to_long <- function(data) {
  data_long <- data %>%
    tidyr::pivot_longer(
      cols = -c(country, countryiso3code),
      names_to = "combined",
      values_to = "value"
    ) %>%
    mutate(
      variable = sub("_(\\d{4})$", "", combined),  # Extract everything before the last underscore
      year = as.numeric(sub(".*_(\\d{4})$", "\\1", combined))  # Extract the year after the last underscore
    ) %>%
    select(-combined)  # Drop the combined column
  return(data_long)
}

#' Impute Missing Values in Long Data
#'
#' Imputes missing values using linear interpolation and global mean imputation for remaining NAs.
#'
#' @param data_long A long-format data frame with columns: `country`, `variable`, `year`, and `value`.
#' @return A long-format data frame with missing values imputed.
#' @export
impute_long_data <- function(data_long) {
  data_long <- data_long %>%
    group_by(country, variable) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(
      value = zoo::na.approx(value, na.rm = FALSE, rule = 2)  # Linear interpolation
    ) %>%
    ungroup() %>%
    group_by(variable) %>%
    mutate(
      value = ifelse(is.na(value), mean(value, na.rm = TRUE), value)  # Global mean imputation
    ) %>%
    ungroup()
  return(data_long)
}

#' Reshape Data to Wide Format
#'
#' Converts long-format data to wide-format, creating variable-year column names.
#'
#' @param data_long A long-format data frame with columns: `country`, `variable`, `year`, and `value`.
#' @return A wide-format data frame with variable-year columns.
#' @export
reshape_to_wide <- function(data_long) {
  data_wide <- data_long %>%
    tidyr::pivot_wider(
      names_from = c("variable", "year"),
      values_from = "value",
      names_glue = "{variable}_{year}"
    )
  return(data_wide)
}
