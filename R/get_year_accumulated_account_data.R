#' Get account data accumulated in a year for specifics accounts in specified months
#' @param account vector indicating the account, Null for all accounts
#' @return tibble with account data.
#' @examples
#' get_year_accumulated_account_data(account= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)")
#' @export

get_year_accumulated_account_data <- function(account= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)"){

  df_trabalho<- get_full_data()

  if (!is.null(account)){

    df_trabalho <-
      df_trabalho %>%
      dplyr::filter(Rubrica %in% account)
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
