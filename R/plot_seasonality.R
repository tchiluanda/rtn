#' Generates a seasonality graphic for a given account data
#'
#' @param .data tibble with 12 month accumulated values
#' @param value_type character 1- For updated value
#' @param clean_names logical cleans the legend name
#' @param clean_names logical uses polar coordinates
#' @return line graph.
#' @examples
#' get_account_data_by_month(c("física"), month = c(1:12), match_required = FALSE) %>% plot_seasonality()
#' @export


plot_seasonality<- function(.data, value_type="1", clean_names = TRUE, polar = TRUE){

  column<- ifelse(value_type==1, "valor_atualizado", "valor_historico")
  texto_eixo_y<- ifelse(value_type==1, "valor atualizado", "valor histórico")

  if (clean_names){

    .data<-
      .data%>%
      dplyr::mutate(Rubrica =  stringr::str_trim( stringr::str_remove_all(Rubrica,"[:punct:]|[0-9]|[:symbol:]")))

  }

  values<-
    .data %>%
    dplyr::select(!!sym(column))

  conta<- unique(.data$Rubrica)

  if (length(conta)>1){
    stop("Error. |The dataset must refer to just one account")
  }

  forecast::ggseasonplot(x= ts(data = values,frequency = 12, start = c(1997,1)),  polar = polar)+
    ggplot2::ylab("R$ milhões") +
    ggplot2::ggtitle(paste0("Seasonality graph - ",conta)) +
    viridis::scale_color_viridis(discrete = TRUE)+
    ggplot2::theme_light() +
    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill= "black") ,
      axis.text.x =  ggplot2::element_text(color = "white")
    )

}
