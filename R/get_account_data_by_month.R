#' Get data accumulated for 12 months for specifics accounts in specified months
#' @param account vector indicating the account, Null for all accounts
#' @param month vector indicating the months of reference.
#' @return tibble with account data.
#' @examples
#' get_account_data_by_month(account= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", month = 12)
#' @export

get_account_data_by_month <- function(account= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", month = 12){

  df_trabalho<- get_full_data()

  df_trabalho<-
    df_trabalho %>%
    dplyr::filter(lubridate::month(Data) %in% month)



  if (!is.null(account)){

    df_trabalho <-
      df_trabalho %>%
      dplyr::filter(Rubrica %in% account)
  }

  df_trabalho

}
