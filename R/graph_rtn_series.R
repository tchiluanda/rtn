#' Generate a line graph for a given accounts data
#'
#' @param .data tibble with 12 month accumulated values
#' @param value_type character 1- For updated value
#' @return line graph.
#' @examples
#' graph_rtn_series(c("3. RECEITA LÍQUIDA","despesa total","acima da linha"), month = c(1:12), match_required = FALSE) %>% graph_rtn_series()
#' @export


graph_rtn_series<- function(.data, value_type="1", clean_names = TRUE){

  column<- ifelse(value_type==1, "valor_atualizado", "valor_historico")
  texto_eixo_y<- ifelse(value_type==1, "valor atualizado", "valor histórico")

  if (clean_names){

    .data<-
      .data%>%
      mutate(Rubrica =  stringr::str_trim( stringr::str_remove_all(Rubrica,"[:punct:]|[0-9]")))

  }

  .data%>%
  ggplot() +
    geom_line(aes(x=Data, y=!!sym(column), color = stringr::str_wrap(Rubrica,20)), size=0.8)+
    theme_light()+
    theme(
      panel.grid = element_blank()
    )+
    labs(color = "Rubrica",
         y= texto_eixo_y)

}
