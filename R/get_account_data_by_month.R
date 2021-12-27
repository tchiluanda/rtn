#' Get RTN revenue data by year
#' @param account vector indicating the account, Null for all accounts
#' @param month vector indicating the months of reference.
#' @return tibble with account data.
#' @examples
#' get_account_data_by_month(account= "8. RESULTADO PRIMÁRIO DO GOVERNO CENTRAL - ABAIXO DA LINHA (5 + 6 + 7)", month = 12)
#' @export

get_account_data_by_month <- function(account= "8. RESULTADO PRIMÁRIO DO GOVERNO CENTRAL - ABAIXO DA LINHA (5 + 6 + 7)", month = 12){

  df_trabalho<- get_full_data()

  df_trabalho<-
    df_trabalho %>%
    filter(lubridate::month(Data) %in% month)



  if (!is.null(account)){

    df_trabalho <-
      df_trabalho %>%
      filter(Rubrica == account)
  }

  df_trabalho

}
