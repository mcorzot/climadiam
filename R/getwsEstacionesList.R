#' @title Obtener la lista de estaciones meteorológicas de la base de datos del CLIMA
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de Andalucía
#' para obtener la lista de estaciones meteorológicas de la base de datos CLIMA.
#'
#' @param idsesion Character. Identificador de sesión válido proporcionado por \code{getwsIDSesion}.
#'
#' @return data.frame con columnas: \code{ACLPKACL}, \code{AGEPKAGE}, \code{CESTACION}, \code{COMPKCOM}, etcétera.
#'
#' @examples
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' estaciones <- getwsEstacionesList(idsesion)
#' head(estaciones)
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @importFrom plyr ldply
#' @export
#'

getwsEstacionesList <- function(idsesion) {

  url <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getEstacionesList&pksesion=", idsesion
  )
  xml_text <- call_with_retry(soap_get(url),"soap_get(getEstacionesList)")
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
    ENTORNO = unlist(extract_nodes("ENTORNO")),
    ESTAPKESTA = unlist(extract_nodes("ESTAPKESTA")),
    FECHAHORAPRO = unlist(extract_nodes("FECHAHORAPRO")),
    FECHAHORAULTAGRE = unlist(extract_nodes("FECHAHORAULTAGRE")),
    GESPKGES = unlist(extract_nodes("GESPKGES")),
    GRAFUBICACION = unlist(extract_nodes("GRAFUBICACION")),
    LATITUD = unlist(extract_nodes("LATITUD")),
    LONGITUD = unlist(extract_nodes("LONGITUD")),
    MODELO = unlist(extract_nodes("MODELO")),
    MUNMUNICIPIO = unlist(extract_nodes("MUNMUNICIPIO")),
    MUNPROVINCIA = unlist(extract_nodes("MUNPROVINCIA")),
    OBSERVACION = unlist(extract_nodes("OBSERVACION")),
    OBSERVACIONADM = unlist(extract_nodes("OBSERVACIONADM")),
    PKEST = unlist(extract_nodes("PKEST")),
    PLUVIO = unlist(extract_nodes("PLUVIO")),
    REDPKRED = unlist(extract_nodes("REDPKRED")),
    WEB = unlist(extract_nodes("WEB")),
    stringsAsFactors = FALSE
  )
  return(df)
}
