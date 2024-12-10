# WorldbankAnalysis

The `WorldbankAnalysis` package provides tools for fetching, processing, and analyzing data from the World Bank API. It includes functionality to handle missing values and build predictive models using machine learning.

## Features

- Fetch and merge World Bank indicators from the API.
- Handle missing values with linear interpolation and global mean imputation.
- Train and evaluate machine learning models on World Bank data.
- Generate informative visualizations and metrics.

## Installation

To install the development version of the package from GitHub:

```r
# Install the devtools package if not already installed
install.packages("devtools")

# Install WorldbankAnalysis
devtools::install_github("ggui6809/WorldbankAnalysis")
