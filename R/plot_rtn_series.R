#' Generate a line graph for given accounts data
#'
#' @param .data tibble with 12 month accumulated values
#' @param value_type character 1- For updated value
#' @param clean_names logical cleans the legend name
#' @return line graph.
#' @examples
#' get_year_accumulated_account_data(c("3. RECEITA LÍQUIDA","despesa total","acima da linha"), match_required = FALSE) %>% plot_rtn_series()
#' @export


plot_rtn_series<- function(.data, value_type="1", clean_names = TRUE){

  column<- ifelse(value_type==1, "valor_atualizado", "valor_historico")
  texto_eixo_y<- ifelse(value_type==1, "valor atualizado", "valor histórico")

  if (clean_names){

    .data<-
      .data%>%
      dplyr::mutate(Rubrica =  stringr::str_trim( stringr::str_remove_all(Rubrica,"[:punct:]|[0-9]")))

  }

  .data%>%
  ggplot2::ggplot() +
    ggplot2::geom_line(aes(x=Data, y=!!sym(column), color = stringr::str_wrap(Rubrica,20)), size=0.8)+
    ggplot2::theme_light()+
    ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )+
    ggplot2::labs(color = "Rubrica",
         y= texto_eixo_y)

}
