
# rtn

<!-- badges: start -->
<!-- badges: end -->

The goal of rtn is to retrieve data related to the reports about the Brazilian Central Government primary results

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tchiluanda/rtn")
```

## Example

See how you can draw a polar graph of time-series related to a given account:

``` r
get_account_data_by_month(c("física"), month = c(1:12), match_required = FALSE) %>%
  plot_seasonality (value_type = "1")


```

## One important note

All the values are in R$ millions

