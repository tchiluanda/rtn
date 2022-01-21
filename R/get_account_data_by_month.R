#' Get data for specifics accounts in specified months
#' @param .data vector indicating the account, Null for all accounts
#' @param month vector indicating the months of reference.
#' @param match_required logic indicates if the account names must match
#' @return tibble with account data.
#' @examples
#' get_account_data_by_month(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", month = 12)
#' @export

get_account_data_by_month <- function(.data=NULL, month = 12,  match_required= TRUE){

  df_trabalho<- get_full_data()

  df_trabalho<-
    df_trabalho %>%
    dplyr::filter(lubridate::month(Data) %in% month)



  if (!is.null(.data) & match_required){

    contas<- .data

    df_trabalho <-
      df_trabalho %>%
      dplyr::filter(stringr::str_to_lower(Rubrica) %in% stringr::str_to_lower(contas))
  }

  if (!is.null(.data) & !match_required){

    contas<- str_trim(str_replace(.data,"[(](?<=[(]).*", ""))

    account_filter<- stringr::str_to_lower(str_c(contas,  collapse = "|"))

    df_trabalho <-
      df_trabalho %>%
      dplyr::filter(stringr::str_detect(stringr::str_to_lower(Rubrica), pattern = account_filter))
  }


  df_trabalho

}
