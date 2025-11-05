#' @title Obtener la lista de áreas geográficas de la base de datos del CLIMA
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de Andalucía
#' para obtener la lista de las áreas geográficas en la base de datos CLIMA.
#'
#' @param idsesion Character. Identificador de sesión válido proporcionado por \code{getwsIDSesion}.
#'
#' @return data.frame con columnas: \code{CODIGO}, \code{DENOMINACION}, \code{PKAGE}.
#'
#' @examples
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' areas_geograficas <- getwsAreasGeograficasList(idsesion)
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @importFrom plyr ldply
#' @export
#'
getwsAreasGeograficasList <- function(idsesion) {
  # Construir la URL SOAP
  url_getAreasGeograficasList <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getAreasGeograficasList&pksesion=", idsesion
  )

  # Descargar el XML como texto (sin escribir a disco)
  xml_text <- soap_get(url_getAreasGeograficasList)

  # Parsear el XML
  doc <- XML::xmlParse(xml_text)

  # Extraer los nodos de cada campo
  codigo <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//CODIGO"), stringsAsFactors = FALSE)
  denominacion <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//DENOMINACION"), stringsAsFactors = FALSE)
  pkage <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//PKAGE"), stringsAsFactors = FALSE)

  # Combinar en un unico data.frame
  df <- data.frame(
    CODIGO = unlist(codigo),
    DENOMINACION = unlist(denominacion),
    PKAGE = unlist(pkage),
    stringsAsFactors = FALSE
  )

  return(df)
}

