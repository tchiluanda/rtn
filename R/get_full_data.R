#' Get all the available data about RTN
#'
#' @return tibble with all.
#' @examples
#' get_full_data()
#' @export

get_full_data <- function(){

  if (NROW(pkg.env$dados_rtn) != 0){

    return(pkg.env$dados_rtn)
  }

  tb_ckan<- ckanr::resource_show(id="527ccdb1-3059-42f3-bf23-b5e3ab4c6dc6",url="http://www.tesourotransparente.gov.br/ckan")
  URL_add <- tb_ckan$url

  tmp <- tempfile(fileext = ".xlsx")
  download.file(URL_add,mode = "wb", destfile = tmp, extra = "-R", method = "libcurl")

  rtn_receita<- readxl::read_xlsx(tmp,sheet = 4,skip = 4,n_max = 57)
  rtn_despesa<- readxl::read_xlsx(tmp,sheet = 4,skip = 62,n_max = 92,col_names = FALSE )
  rtn_geral <- readxl::read_xlsx(tmp,sheet = 2,skip = 64,n_max = 8)

  names(rtn_receita)[1]<-"Rubrica"
  names(rtn_despesa)[1]<-"Rubrica"
  names(rtn_geral)[1]<-"Rubrica"

  #plano_contas<- tibble::tibble(Rubrica=c(rtn_receita$Rubrica, rtn_despesa$Rubrica, rtn_geral$Rubrica))


  # plano_contas<-
  #   plano_contas %>%
  #   dplyr::mutate(id=  dplyr::row_number()) %>%
  #   dplyr::select(Rubrica, id)



  deflator_IPCA <- readxl::read_xlsx(tmp,sheet = 3,skip = 74,n_max = 1, col_names = FALSE)
  names(deflator_IPCA)<-names(rtn_receita)
  names(rtn_despesa) <-names(rtn_receita)

  rtn_receita$id <- 1:NROW(rtn_receita)
  series_temporais_analise_rec<-tidyr::gather(rtn_receita,Data, Valor,c(-Rubrica, -id))
  series_temporais_analise_rec$Data<-as.Date(as.numeric(series_temporais_analise_rec$Data), origin="1899-12-30")
  series_temporais_analise_rec$Valor <-round(as.numeric(series_temporais_analise_rec$Valor),0)
  series_temporais_analise_rec$Valor[is.na(series_temporais_analise_rec$Valor)]<-0
  series_temporais_analise_rec$tipo <- "R"

  proximo_id <- NROW(rtn_receita) +1
  ultimo_id <- proximo_id + NROW(rtn_despesa) -1


  rtn_despesa$id <- proximo_id:ultimo_id
  series_temporais_analise_desp<-tidyr::gather(rtn_despesa,Data, Valor,c(-Rubrica, -id))
  series_temporais_analise_desp$Data<-as.Date(as.numeric(series_temporais_analise_desp$Data), origin="1899-12-30")
  series_temporais_analise_desp$Valor <-round(as.numeric(series_temporais_analise_desp$Valor),0)
  series_temporais_analise_desp$Valor[is.na(series_temporais_analise_desp$Valor)]<-0
  series_temporais_analise_desp$tipo <- "D"


  proximo_id <- ultimo_id +1
  ultimo_id <- proximo_id + NROW(rtn_geral) -1
  rtn_geral$id <- proximo_id:ultimo_id

  names(rtn_geral)<-names(rtn_receita)
  names(rtn_geral)[1]<-"Rubrica"


  series_temporais_analise<-gather(rtn_geral,Data, Valor,c(-Rubrica, -id))
  series_temporais_analise$Data<-as.Date(as.numeric(series_temporais_analise$Data), origin="1899-12-30")
  series_temporais_analise$Valor <-round(as.numeric(series_temporais_analise$Valor),0)
  series_temporais_analise$Valor[is.na(series_temporais_analise$Valor)]<-0

  names(deflator_IPCA)[1]<-"Rubrica"
  series_temporais_analise_IPCA<-tidyr::gather(deflator_IPCA,Data, Valor,c(-Rubrica))
  series_temporais_analise_IPCA$Data<-as.Date(as.numeric(series_temporais_analise_IPCA$Data), origin="1899-12-30")
  series_temporais_analise_IPCA$Valor <-as.numeric(series_temporais_analise_IPCA$Valor)

  serie_completa<-
    series_temporais_analise_rec %>%
    dplyr::bind_rows(series_temporais_analise_desp,
                     series_temporais_analise)

  serie_completa<-
      serie_completa %>%
      dplyr::mutate(valor_historico = Valor) %>%
      dplyr::inner_join(
        series_temporais_analise_IPCA %>%
          dplyr::mutate(deflator =  Valor) %>%
          dplyr::select(Data, deflator), by = "Data") %>%
      dplyr::mutate(valor_atualizado = deflator * valor_historico ) %>%
    arrange(Data, id, Rubrica) %>%
    select(Data,Rubrica, id, tipo,valor_historico, valor_atualizado )



  pkg.env$dados_rtn<- serie_completa


  serie_completa
}
