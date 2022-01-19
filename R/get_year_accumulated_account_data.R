#' Get account data accumulated in a year for specifics accounts in specified months
#' @param .data vector indicating the account, Null for all accounts
#' @param match_required logic indicates if the account names must match
#' @return tibble with account data.
#' @examples
#' get_year_accumulated_account_data(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)")
#' @export

get_year_accumulated_account_data <- function(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", match_required= TRUE){

  df_trabalho<- get_full_data()

  if (!is.null(.data) & match_required){

    df_trabalho <-
      df_trabalho %>%
      dplyr::filter(Rubrica %in% .data)
  }

  if (!is.null(.data) & !match_required){

    account_filter<- str_to_lower(str_c(.data,  collapse = "|"))

    df_trabalho <-
      df_trabalho %>%
      dplyr::filter(str_detect(str_to_lower(Rubrica), pattern = account_filter))
  }




  df_trabalho<-
    df_trabalho %>%
    mutate(ano = lubridate::year(Data)) %>%
    group_by( ano, id, Rubrica) %>%
    summarise(
      valor_historico_anual=sum(valor_historico),
      valor_atualizado_anual = sum(valor_atualizado)
    ) %>%
    ungroup()


  df_trabalho

}
