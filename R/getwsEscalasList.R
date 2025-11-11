#' @title Obtener la lista de las escalas temporales de la base de datos del CLIMA
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de Andalucía
#' para obtener la lista de las escalas temporales en la base de datos CLIMA.
#'
#' @param idsesion Character. Identificador de sesión válido proporcionado por \code{getwsIDSesion}.
#'
#' @return data.frame con columnas: \code{CESCALA}, \code{DENOMINACION}, \code{PKESC}.
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' escalas <- getwsEscalasList(idsesion)
#' head(escalas)
#' }
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @importFrom plyr ldply
#' @export
#'
getwsEscalasList <- function(idsesion) {
  # Construir la URL SOAP
  url_getEscalasList <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getEscalasList&pksesion=", idsesion
  )

  # Descargar el XML como texto (sin escribir a disco)
  xml_text <- call_with_retry(soap_get(url_getEscalasList),"soap_get(getEscalasList)")

  # Parsear el XML
  doc <- XML::xmlParse(xml_text)

  # Extraer los nodos de cada campo
  cescala <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//CESCALA"), stringsAsFactors = FALSE)
  denominacion <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//DENOMINACION"), stringsAsFactors = FALSE)
  pkesc <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//PKESC"), stringsAsFactors = FALSE)

  # Combinar en un unico data.frame
  df <- data.frame(
    CESCALA = unlist(cescala),
    DENOMINACION = unlist(denominacion),
    PKESC = unlist(pkesc),
    stringsAsFactors = FALSE
  )

  return(df)
}

