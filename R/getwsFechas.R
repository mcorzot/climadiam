#' @title Obtener el identificador de una fecha de la base de datos del CLIMA
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de Andalucía
#' para obtener el identificador de una fecha de la base de datos CLIMA.
#'
#' @param date fecha en formato DD/MM/YYYY
#' @param idsesion Character. Identificador de sesión válido proporcionado por \code{getwsIDSesion}.
#'
#' @return data.frame con columnas: \code{PKFEC}, \code{FECHA}.
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' date <- "20/10/2025"
#' fecha <- getwsFechas(date,idsesion)
#' head(fecha)
#' }
#'
#' @importFrom XML xmlParse xmlToDataFrame getNodeSet
#' @importFrom RCurl getURL
#' @importFrom plyr ldply
#' @export
#'
getwsFechas <- function(date, idsesion) {
  url <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getFechas&pksesion=", idsesion, "&FECHA=", date
  )
  xml_text <- call_with_retry(soap_get(url),"soap_get(getFechas)")
  doc <- xmlParse(xml_text)
  pkfec <- xmlToDataFrame(nodes = XML::getNodeSet(doc, "//PKFEC"), stringsAsFactors = FALSE)
  fecha <- xmlToDataFrame(nodes = XML::getNodeSet(doc, "//FECHA"), stringsAsFactors = FALSE)
  df <- data.frame(PKFEC = unlist(pkfec), FECHA = unlist(fecha), stringsAsFactors = FALSE)

  # Aviso en caso de que no se encuentre la date
  if(nrow(df) == 0){
    message(paste0("No se ha encontrado la fecha ",date))
  } else {
    message(paste0("Obtenido dataframe de fechas con ",nrow(df)," registros."))
  }

  return(df)
}
