% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute_utilities.R
\name{reshape_to_wide}
\alias{reshape_to_wide}
\title{Reshape Data to Wide Format}
\usage{
reshape_to_wide(data_long)
}
\arguments{
\item{data_long}{A long-format data frame with columns: \code{country}, \code{variable}, \code{year}, and \code{value}.}
}
\value{
A wide-format data frame with variable-year columns.
}
\description{
Converts long-format data to wide-format, creating variable-year column names.
}
