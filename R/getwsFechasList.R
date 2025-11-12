#' @title Obtener la lista de fechas de la base de datos del CLIMA
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de Andalucía
#' para obtener la lista de las fechas en la base de datos CLIMA.
#'
#' @param idsesion Character. Identificador de sesión válido proporcionado por \code{getwsIDSesion}.
#'
#' @return data.frame con columnas: \code{CRED}, \code{DENOMINACION}, \code{OBSERVACION}, \code{PKRED}, \code{URL}.
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' fechas <- getwsFechasList(idsesion)
#' head(fechas)
#' }
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @importFrom plyr ldply
#' @export
#'
getwsFechasList <- function(idsesion) {
  # Construir la URL SOAP
  url_getFechasList <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getFechasList&pksesion=", idsesion
  )

  # Descargar el XML como texto (sin escribir a disco)
  xml_text <- call_with_retry(soap_get(url_getFechasList),"soap_get(getFechasList)")

  # Parsear el XML
  doc <- XML::xmlParse(xml_text)

  # Extraer los nodos de cada campo
  fecha <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//FECHA"), stringsAsFactors = FALSE)
  pkfec <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//PKFEC"), stringsAsFactors = FALSE)

  # Combinar en un unico data.frame
  df <- data.frame(
    FECHA = unlist(fecha),
    PKFEC = unlist(pkfec),
    stringsAsFactors = FALSE
  )

  message(paste0("Obtenido dataframe de fechas con ",nrow(df)," registros."))

  return(df)
}

