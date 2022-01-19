#' Get account data accumulated for 12 months for specifics accounts in specified months
#' @param .data vector indicating the account, Null for all accounts
#' @param month vector indicating the months of reference. Null for all months
#' @param match_required logic indicates if the account names must match
#' @return tibble with account data.
#' @examples
#' get_12_month_accumulated_account_data_by_month(.data= "5. RESULTADO PRIMÁRIO GOVERNO CENTRAL - ACIMA DA LINHA (3 - 4)", month = NULL)
#' @export

get_12_month_accumulated_account_data_by_month <- function(.data= NULL, month = NULL){

  df_trabalho<- get_full_data()

  # if (!is.null(.data) & match_required){
  #
  #   df_trabalho <-
  #     df_trabalho %>%
  #     dplyr::filter(stringr::str_to_lower(Rubrica) %in% stringr::str_to_lower(.data))
  # }
  #
  # if (!is.null(.data) & !match_required){
  #
  #   account_filter<- stringr::str_to_lower(str_c(.data,  collapse = "|"))
  #
  #   df_trabalho <-
  #     df_trabalho %>%
  #     dplyr::filter(stringr::str_detect(stringr::str_to_lower(Rubrica), pattern = account_filter))
  # }

  account_filter<- stringr::str_to_lower(str_c(.data,  collapse = "|"))

  df_trabalho <-
    df_trabalho %>%
    dplyr::filter(stringr::str_detect(stringr::str_to_lower(Rubrica), pattern = account_filter))




  datas_rubricas<-
  df_trabalho %>%
    dplyr::filter(Data>="1997-12-01" ) %>%
    dplyr::distinct(Data, id, Rubrica) %>%
    dplyr::select(Data, id, Rubrica) %>%
    dplyr::arrange(id,Data)

  saldos_acumu<-
  df_trabalho %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(
      valor_historico_acum= zoo::rollsum(valor_historico,12,align = "right"),
      valor_atualizado_acum=zoo::rollsum(valor_atualizado,12,align = "right")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-id)


  df_trabalho <-
    datas_rubricas %>%
    dplyr::bind_cols(saldos_acumu) %>%
    dplyr::arrange(Data,id,Rubrica)

  if (!is.null(month)){

    df_trabalho<-
      df_trabalho %>%
      dplyr::filter(lubridate::month(Data) %in% month)
  }
  df_trabalho

}
