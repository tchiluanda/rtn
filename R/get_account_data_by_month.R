#' Get data for specifics accounts in specified months
#' @param account vector indicating the account, Null for all accounts
#' @param month vector indicating the months of reference.
#' @param match_required logic indicates if the account names must match
#' @return tibble with account data.
#' @examples
#' get_account_data_by_month(account= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", month = 12)
#' @export

get_account_data_by_month <- function(account= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", month = 12,  match_required= TRUE){

  df_trabalho<- get_full_data()

  df_trabalho<-
    df_trabalho %>%
    dplyr::filter(lubridate::month(Data) %in% month)



  if (!is.null(account) & match_required){

    df_trabalho <-
      df_trabalho %>%
      dplyr::filter(stringr::str_to_lower(Rubrica) %in% stringr::str_to_lower(account))
  }

  if (!is.null(account) & !match_required){


    account_filter<- str_to_lower(str_c(account,  collapse = "|"))

    df_trabalho <-
      df_trabalho %>%
      dplyr::filter(str_detect(str_to_lower(Rubrica), pattern = account_filter))
  }


  df_trabalho

}
