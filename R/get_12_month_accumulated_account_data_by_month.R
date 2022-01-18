#' Get account data accumulated for 12 months for specifics accounts in specified months
#' @param account vector indicating the account, Null for all accounts
#' @param month vector indicating the months of reference. Null for all months
#' @param match_required logic indicates if the account names must match
#' @return tibble with account data.
#' @examples
#' get_12_month_accumulated_account_data_by_month(account= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", month = NULL)
#' @export

get_12_month_accumulated_account_data_by_month <- function(account= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", month = NULL, match_required= TRUE){

  df_trabalho<- get_full_data()

  if (!is.null(account) & match_required){

    df_trabalho <-
      df_trabalho %>%
      dplyr::filter(str_to_lower(Rubrica) %in% str_to_lower(account))
  }

  if (!is.null(account) & !match_required){

    account_filter<- str_to_lower(str_c(account,  collapse = "|"))

    df_trabalho <-
      df_trabalho %>%
      dplyr::filter(str_detect(str_to_lower(Rubrica), pattern = account_filter))
  }



  datas_rubricas<-
  df_trabalho %>%
    filter(Data>="1997-12-01" ) %>%
    dplyr::distinct(Data, id, Rubrica) %>%
    select(Data, id, Rubrica) %>%
    dplyr::arrange(id,Data)

  saldos_acumu<-
  df_trabalho %>%
    dplyr::group_by(id) %>%
    summarise(
      valor_historico_acum= zoo::rollsum(valor_historico,12,align = "right"),
      valor_atualizado_acum=zoo::rollsum(valor_atualizado,12,align = "right")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-id)


  df_trabalho <-
    datas_rubricas %>%
    bind_cols(saldos_acumu) %>%
    arrange(Data,id,Rubrica)

  if (!is.null(month)){

    df_trabalho<-
      df_trabalho %>%
      dplyr::filter(lubridate::month(Data) %in% month)
  }
  df_trabalho

}
