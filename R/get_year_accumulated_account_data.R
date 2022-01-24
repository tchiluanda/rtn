#' Get account data accumulated in a year for specifics accounts in specified months
#' @param .data vector indicating the account, Null for all accounts
#' @param match_required logic indicates if the account names must match
#' @return tibble with account data.
#' @examples
#' get_year_accumulated_account_data(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)")
#' @export

get_year_accumulated_account_data <- function(.data= NULL, match_required= TRUE){

  df_trabalho<- get_full_data()

  if (!is.null(.data) & match_required){

    contas<- .data

    df_trabalho <-
      df_trabalho %>%
      dplyr::filter(stringr::str_to_lower(Rubrica) %in% stringr::str_to_lower(contas))
  }

  if (!is.null(.data) & !match_required){

    contas<- stringr::str_trim(stringr::str_replace(.data,"[(](?<=[(]).*", ""))

    account_filter<- stringr::str_to_lower(stringr::str_c(contas,  collapse = "|"))

    df_trabalho <-
      df_trabalho %>%
      dplyr::filter(stringr::str_detect(stringr::str_to_lower(Rubrica), pattern = account_filter))
  }

  df_trabalho<-
    df_trabalho %>%
    dplyr::mutate(Data = lubridate::year(Data)) %>%
    dplyr::group_by( Data, id, Rubrica) %>%
    dplyr::summarise(
      valor_historico=sum(valor_historico),
      valor_atualizado = sum(valor_atualizado)
    ) %>%
    dplyr::ungroup()


  df_trabalho

}
