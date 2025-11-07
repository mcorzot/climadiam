#' @title Obtener una estación de la lista de estaciones de la base de datos del CLIMA
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de Andalucía
#' para obtener los datos de una estación meteorológica de la base de datos CLIMA.
#'
#' @param cestacion Character. Identificador de la estación.
#' @param idsesion Character. Identificador de sesión válido proporcionado por \code{getwsIDSesion}.
#'
#' @return data.frame con columnas: \code{ACLPKACL}, \code{AGEPKAGE}, \code{CESTACION}, \code{COMPKCOM}, etcétera.
#'
#' @examples
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' cestacion <- 'EARM22'
#' estacion <- getwsEstaciones(cestacion,idsesion)
#' head(estacion)
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @importFrom plyr ldply
#' @export
#'

getwsEstaciones <- function(cestacion,idsesion) {

  url <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getEstaciones&pksesion=", idsesion,"&CESTACION=", cestacion
  )

  xml_text <- call_with_retry(soap_get(url),"soap_get(getEstaciones)")
  doc <- xmlParse(xml_text)

  extract_nodes <- function(tag) {
    xmlToDataFrame(nodes = XML::getNodeSet(doc, paste0("//", tag)), stringsAsFactors = FALSE)
  }

  df <- data.frame(
    ACLPKACL = unlist(extract_nodes("ACLPKACL")),
    AGEPKAGE = unlist(extract_nodes("AGEPKAGE")),
    CESTACION = unlist(extract_nodes("CESTACION")),
    COMPKCOM = unlist(extract_nodes("COMPKCOM")),
    COORDENADAX = unlist(extract_nodes("COORDENADAX")),
    COORDENADAY = unlist(extract_nodes("COORDENADAY")),
    COORDENADAZ = unlist(extract_nodes("COORDENADAZ")),
    DENOMINACION = unlist(extract_nodes("DENOMINACION")),
    ESTAPKESTA = unlist(extract_nodes("ESTAPKESTA")),
    GESPKGES = unlist(extract_nodes("GESPKGES")),
    LATITUD = unlist(extract_nodes("LATITUD")),
    LONGITUD = unlist(extract_nodes("LONGITUD")),
    MODELO = unlist(extract_nodes("MODELO")),
    MUNMUNICIPIO = unlist(extract_nodes("MUNMUNICIPIO")),
    MUNPROVINCIA = unlist(extract_nodes("MUNPROVINCIA")),
    PKEST = unlist(extract_nodes("PKEST")),
    REDPKRED = unlist(extract_nodes("REDPKRED")),
    stringsAsFactors = FALSE
  )
  return(df)
}
