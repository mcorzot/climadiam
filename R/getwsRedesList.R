#' @title Obtener la lista de redes de estaciones meteorológicas (CLIMA)
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de Andalucía
#' para obtener la lista de redes disponibles en la base de datos CLIMA.
#'
#' @param idsesion Character. Identificador de sesión válido proporcionado por \code{getwsIDSesion}.
#'
#' @return data.frame con columnas: \code{CRED}, \code{DENOMINACION}, \code{OBSERVACION}, \code{PKRED}.
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' redes <- getwsRedesList(idsesion)
#' head(redes)
#' }
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @importFrom plyr ldply
#' @export
#'
getwsRedesList <- function(idsesion) {
  # Construir la URL SOAP
  url_getRedesList <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getRedesList&pksesion=", idsesion
  )

  # Descargar el XML como texto (sin escribir a disco)
  xml_text <- call_with_retry(soap_get(url_getRedesList),"soap_get(getRedesList)")

  # Parsear el XML
  doc <- XML::xmlParse(xml_text)

  # Extraer los nodos de cada campo
  cred <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//CRED"), stringsAsFactors = FALSE)
  denominacion <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//DENOMINACION"), stringsAsFactors = FALSE)
  observacion <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//OBSERVACION"), stringsAsFactors = FALSE)
  pkred <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//PKRED"), stringsAsFactors = FALSE)

  # Combinar en un unico data.frame
  df <- data.frame(
    CRED = unlist(cred),
    DENOMINACION = unlist(denominacion),
    OBSERVACION = unlist(observacion),
    PKRED = unlist(pkred),
    stringsAsFactors = FALSE
  )

  message(paste0("Obtenido dataframe de redes de estaciones meteorol\u00F3gicas con ",nrow(df)," registros."))

  return(df)
}

