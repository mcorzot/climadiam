#' @title Obtener la lista de comarcas de la base de datos del CLIMA
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de Andalucía
#' para obtener la lista de las comarcas en la base de datos CLIMA.
#'
#' @param idsesion Character. Identificador de sesión válido proporcionado por \code{getwsIDSesion}.
#'
#' @return data.frame con columnas: \code{AMBITO}, \code{CCOMARCA}, \code{DENOMINACION}, \code{PKCOM}.
#'
#' @examples
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' comarcas <- getwsComarcasList(idsesion)
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @importFrom plyr ldply
#' @export
#'
getwsComarcasList <- function(idsesion) {
  # Construir la URL SOAP
  url_getComarcasList <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getComarcasList&pksesion=", idsesion
  )

  # Descargar el XML como texto (sin escribir a disco)
  xml_text <- call_with_retry(soap_get(url_getComarcasList),"soap_get(getComarcasList)")

  # Parsear el XML
  doc <- XML::xmlParse(xml_text)

  # Extraer los nodos de cada campo
  ambito <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//AMBITO"), stringsAsFactors = FALSE)
  ccomarca <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//CCOMARCA"), stringsAsFactors = FALSE)
  denominacion <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//DENOMINACION"), stringsAsFactors = FALSE)
  pkcom <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//PKCOM"), stringsAsFactors = FALSE)

  # Combinar en un unico data.frame
  df <- data.frame(
    AMBITO = unlist(ambito),
    CCOMARCA = unlist(ccomarca),
    DENOMINACION = unlist(denominacion),
    PKCOM = unlist(pkcom),
    stringsAsFactors = FALSE
  )

  return(df)
}

