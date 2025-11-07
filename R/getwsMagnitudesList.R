#' @title Obtener la lista de magnitudes de la base de datos del CLIMA
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de Andalucía
#' para obtener la lista de magnitudes en la base de datos CLIMA.
#'
#' @param idsesion Character. Identificador de sesión válido proporcionado por \code{getwsIDSesion}.
#'
#' @return data.frame con columnas: \code{CMAGNITUDES}, \code{DENOMINACION}, \code{PKMAG}.
#'
#' @examples
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' magnitudes <- getwsMagnitudesList(idsesion)
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @importFrom plyr ldply
#' @export
#'
getwsMagnitudesList <- function(idsesion) {
  # Construir la URL SOAP
  url_getMagnitudesList <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getMagnitudesList&pksesion=", idsesion
  )

  # Descargar el XML como texto (sin escribir a disco)
  xml_text <- call_with_retry(soap_get(url_getMagnitudesList),"soap_get(getMagnitudesList)")

  # Parsear el XML
  doc <- XML::xmlParse(xml_text)

  # Extraer los nodos de cada campo
  cmagnitudes <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//CMAGNITUDES"), stringsAsFactors = FALSE)
  denominacion <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//DENOMINACION"), stringsAsFactors = FALSE)
  pkmag <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//PKMAG"), stringsAsFactors = FALSE)

  # Combinar en un unico data.frame
  df <- data.frame(
    CMAGNITUDES = unlist(cmagnitudes),
    DENOMINACION = unlist(denominacion),
    PKMAG = unlist(pkmag),
    stringsAsFactors = FALSE
  )

  return(df)
}

