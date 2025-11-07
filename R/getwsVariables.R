#' @title Obtener una variable de la lista de variables  meteorológicas de la base de datos del CLIMA
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de Andalucía
#' para obtener una variable de la lista de variables meteorológicas de la base de datos CLIMA.
#'
#' @param cvariable Character. Identificador de la variable.
#' @param idsesion Character. Identificador de sesión válido proporcionado por \code{getwsIDSesion}.
#'
#' @return data.frame con columnas: \code{CVARIABLE}, \code{DENOMINACION}, \code{FRECUENCIA}, \code{PKVAR},  \code{UDMPKUDM}.
#'
#' @examples
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' cvariable <- 'TI1'
#' variable <- getwsVariables(cvariable,idsesion)
#' head(variable)
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @importFrom plyr ldply
#' @export
#'
getwsVariables <- function(cvariable,idsesion) {

  url <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getVariables&pksesion=", idsesion,"&CVARIABLE=", cvariable
  )
  xml_text <- call_with_retry(soap_get(url),"soap_get(getVariables)")
  doc <- xmlParse(xml_text)
  extract_nodes <- function(tag) xmlToDataFrame(nodes = XML::getNodeSet(doc, paste0("//", tag)), stringsAsFactors = FALSE)

  data.frame(
    CVARIABLE = unlist(extract_nodes("CVARIABLE")),
    DENOMINACION = unlist(extract_nodes("DENOMINACION")),
    FRECUENCIA = unlist(extract_nodes("FRECUENCIA")),
    PKVAR = unlist(extract_nodes("PKVAR")),
    UDMPKUDM = unlist(extract_nodes("UDMPKUDM")),
    stringsAsFactors = FALSE
  )
}
