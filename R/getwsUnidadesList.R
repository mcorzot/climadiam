#' @title Obtener la lista de unidades de medida de la base de datos del CLIMA
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de Andalucía
#' para obtener la lista de unidades de medida en la base de datos CLIMA.
#'
#' @param idsesion Character. Identificador de sesión válido proporcionado por \code{getwsIDSesion}.
#'
#' @return data.frame con columnas: \code{ABREVIATURA}, \code{DENOMINACION}, \code{PKUDM}.
#'
#' @examples
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' unidades <- getwsUnidadesList(idsesion)
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @importFrom plyr ldply
#' @export
#'
getwsUnidadesList <- function(idsesion) {
  # Construir la URL SOAP
  url_getUnidadesList <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getUnidadesList&pksesion=", idsesion
  )

  # Descargar el XML como texto (sin escribir a disco)
  xml_text <- soap_get(url_getUnidadesList)

  # Parsear el XML
  doc <- XML::xmlParse(xml_text)

  # Extraer los nodos de cada campo
  abreviatura <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//ABREVIATURA"), stringsAsFactors = FALSE)
  denominacion <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//DENOMINACION"), stringsAsFactors = FALSE)
  pkudm <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//PKUDM"), stringsAsFactors = FALSE)

  # Combinar en un unico data.frame
  df <- data.frame(
    ABREVIATURA = unlist(abreviatura),
    DENOMINACION = unlist(denominacion),
    PKUDM = unlist(pkudm),
    stringsAsFactors = FALSE
  )

  return(df)
}

