#' Fetch Paginated Data
#'
#' Fetches paginated data from a given API URL.
#'
#' @param url A URL for a World Bank API endpoint.
#' @return A tibble containing the combined paginated data.
#' @export
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
