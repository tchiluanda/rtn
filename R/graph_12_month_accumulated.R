#' Generate a graph showing a the evolution of 12 month accumulated values
#'
#' @param .data tibble with 12 month accumulated values
#' @param value_type character 1- For updated value
#' @return line graph.
#' @examples
#'
#' @export


graph_12_month_accumulated<- function(.data, value_type="1"){

  column<- ifelse(value_type==1, "valor_atualizado_acum", "valor_historico_acum")
  texto_eixo_y<- ifelse(value_type==1, "valor atualizado", "valor histórico")

  .data%>%
  ggplot() +
    geom_line(aes(x=Data, y=!!sym(column), color = str_wrap(Rubrica,20)), size=0.8)+
    theme_light()+
    theme(
      panel.grid = element_blank()
    )+
    labs(color = "Rubrica",
         y= texto_eixo_y)

}
