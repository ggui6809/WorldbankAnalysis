#' Fetch and Merge Indicators
#'
#' Fetches and processes multiple World Bank indicators and merges them into a single dataset.
#'
#' @param indicator_urls A list of World Bank API URLs for indicators.
#' @param num_cores Number of cores for parallel processing. Default is all available cores minus one.
#' @param start_year The start year for filtering the data. Default is 2015.
#' @param end_year The end year for filtering the data. Default is 2022.
#' @return A merged dataset containing all the processed indicators.
#' @export
#' @importFrom dplyr filter mutate group_by ungroup select arrange
#' @importFrom tidyr pivot_longer pivot_wider
fetch_merge_indicators <- function(indicator_urls, num_cores = 1, start_year = 2015, end_year = 2022) {
  # Fetch and process indicator data
  indicator_data <- fetch_and_process_indicators(indicator_urls, num_cores)

  # Merge all indicators into a single dataset
  merged_data <- merge_all_indicators(indicator_data)

  # Filter data for specified year range
  filtered_data <- merged_data %>%
    filter(as.numeric(date) >= start_year & as.numeric(date) <= end_year)

  # Get the variable names dynamically from the indicator_urls
  variables <- names(indicator_urls)

  # Pivot data for analysis
  expanded_data <- filtered_data %>%
    tidyr::pivot_wider(
      names_from = date,
      values_from = all_of(variables),
      names_glue = "{.value}_{date}"
    )

  return(expanded_data)
}
