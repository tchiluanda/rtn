#' Get the full account name related to a given part of an account name
#' @param account vector indicating part of the account name, Null for all accounts
#' @return tibble with account data.
#' @examples
#' get_full_account_name(account= "resultado")
#' @export

get_full_account_name <- function(account= NULL){

  df_trabalho<- get_full_data()


  if (!is.null(account)){

    account_filter<- str_to_lower(str_c(account,  collapse = "|"))

    df_trabalho <-
      df_trabalho %>%
      dplyr::filter(str_detect(str_to_lower(Rubrica), pattern = account_filter))
  }

  (df_trabalho %>%
    distinct(Rubrica))$Rubrica

}
