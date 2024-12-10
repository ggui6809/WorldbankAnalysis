#' Fetch and Process Indicators
#'
#' Fetch and process World Bank indicators using parallel processing.
#'
#' @param indicator_urls List of URLs for World Bank indicators.
#' @param num_cores Number of cores to use for parallel processing. Defaults to all available cores minus one.
#' @return A list of tibbles with processed data for each indicator.
#' @export
#' @importFrom httr GET content status_code
#' @importFrom dplyr bind_rows rename
#' @importFrom parallel makeCluster clusterExport clusterEvalQ parLapply stopCluster detectCores
fetch_and_process_indicators <- function(indicator_urls, num_cores = parallel::detectCores() - 1) {
  # Function to fetch paginated data
  fetch_paginated_data <- function(url) {
    page <- 1
    all_data <- list()
    repeat {
      paged_url <- paste0(url, "&page=", page)
      response <- httr::GET(paged_url)
      if (httr::status_code(response) != 200) {
        warning("Failed to retrieve data from page ", page)
        break
      }
      data <- httr::content(response, "parsed")
      if (length(data[[2]]) == 0) break
      all_data <- c(all_data, data[[2]])
      page <- page + 1
    }
    lapply(all_data, function(record) {
      tibble::tibble(
        country = record$country$value,
        countryiso3code = record$countryiso3code,
        date = record$date,
        value = record$value
      )
    }) %>% dplyr::bind_rows()
  }

  # Parallel fetching of indicator data
  cl <- parallel::makeCluster(num_cores)
  parallel::clusterExport(cl, varlist = c("fetch_paginated_data"))
  parallel::clusterEvalQ(cl, library(httr))
  parallel::clusterEvalQ(cl, library(dplyr))
  indicator_data <- parallel::parLapply(cl, indicator_urls, fetch_paginated_data)
  parallel::stopCluster(cl)

  # Rename the `value` column dynamically for each indicator
  for (name in names(indicator_data)) {
    indicator_data[[name]] <- indicator_data[[name]] %>%
      dplyr::rename(!!name := value)
  }

  return(indicator_data)
}

#' Merge All Indicators
#'
#' Merge multiple indicator datasets into a single dataset.
#'
#' @param indicator_data List of tibbles containing processed indicator data.
#' @return A single merged tibble with all indicators.
#' @export
#' @importFrom dplyr left_join
merge_all_indicators <- function(indicator_data) {
  merged_data <- Reduce(function(x, y) {
    dplyr::left_join(x, y, by = c("country", "countryiso3code", "date"))
  }, indicator_data)
  return(merged_data)
}
