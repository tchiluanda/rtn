
# rtn

<!-- badges: start -->
<!-- badges: end -->

The goal of rtn is to ...

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tchiluanda/rtn")
```

## Example

See how you can draw a polar graph of time-series related to a given account:

``` r
library(rtn)
library(forecast)

account<- "1.1.3.1  I.R. - Pessoa Física"
account_values<- get_account_data_by_month(account = account, month = c(1:12))

ggseasonplot(x= ts(data = account_values$valor_atualizado,frequency = 12, start = c(1997,1)),  polar = TRUE)+
  ylab("R$ milhões") +
  ggtitle(paste0("Gráfico de sazonalidade - ",account))



```

