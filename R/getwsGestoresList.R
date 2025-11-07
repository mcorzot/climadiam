#' @title Obtener la lista de gestores de redes meteorológicas de la base de datos del CLIMA
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de Andalucía
#' para obtener la lista de gestores de redes meteorológicas en la base de datos CLIMA.
#'
#' @param idsesion Character. Identificador de sesión válido proporcionado por \code{getwsIDSesion}.
#'
#' @return data.frame con columnas: \code{CGESTOR}, \code{DENOMINACION}, \code{PKGES}.
#'
#' @examples
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' gestores <- getwsGestoresList(idsesion)
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @importFrom plyr ldply
#' @export
#'
getwsGestoresList <- function(idsesion) {
  # Construir la URL SOAP
  url_getGestoresList <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getGestoresList&pksesion=", idsesion
  )

  # Descargar el XML como texto (sin escribir a disco)
  xml_text <- call_with_retry(soap_get(url_getGestoresList),"soap_get(getGestoresList)")

  # Parsear el XML
  doc <- XML::xmlParse(xml_text)

  # Extraer los nodos de cada campo
  cgestor <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//CGESTOR"), stringsAsFactors = FALSE)
  denominacion <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//DENOMINACION"), stringsAsFactors = FALSE)
  pkges <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//PKGES"), stringsAsFactors = FALSE)

  # Combinar en un unico data.frame
  df <- data.frame(
    CGESTOR = unlist(cgestor),
    DENOMINACION = unlist(denominacion),
    PKGES = unlist(pkges),
    stringsAsFactors = FALSE
  )

  return(df)
}

