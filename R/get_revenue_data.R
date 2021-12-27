#' Get RTN revenue data by year
#' @param year vector indicating the year or reference. NULL for all years
#' @return tibble with all.
#' @examples
#' get_revenue_data(year = 2021)
#' @export

get_revenue_data <- function(year= NULL){

  df_trabalho<- get_full_data()

  df_trabalho <-
    df_trabalho %>%
    filter(tipo == "R")

  if (!is.null(year)){

    df_trabalho<-
      df_trabalho %>%
      filter(lubridate::year(Data) %in% year)

  }

  df_trabalho

}
