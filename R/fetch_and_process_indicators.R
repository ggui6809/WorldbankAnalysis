#' Fetch and Process Indicators
#'
#' Fetch and process World Bank indicators using parallel processing.
#'
#' @param indicator_urls List of URLs for World Bank indicators.
#' @param num_cores Number of cores to use for parallel processing. Defaults to all available cores minus one.
#' @return A list of tibbles with processed data for each indicator.
#' @export
fetch_and_process_indicators <- function(indicator_urls, num_cores = 1) {
  # Parallel fetching of indicator data
  cl <- parallel::makeCluster(num_cores)

  # Export all necessary functions and packages
  parallel::clusterExport(cl, varlist = c("fetch_paginated_data"), envir = environment())
  parallel::clusterEvalQ(cl, library(httr))
  parallel::clusterEvalQ(cl, library(dplyr))
  parallel::clusterEvalQ(cl, library(tidyr))

  # Fetch data in parallel
  indicator_data <- parallel::parLapply(cl, indicator_urls, fetch_paginated_data)
  parallel::stopCluster(cl)

  # Rename the `value` column dynamically for each indicator
  for (name in names(indicator_data)) {
    indicator_data[[name]] <- indicator_data[[name]] %>%
      dplyr::rename(!!name := value)
  }

  return(indicator_data)
}
