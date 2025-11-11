#' @title Obtener la lista de provincias de la base de datos del CLIMA
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de Andalucía
#' para obtener la lista de provincias en la base de datos CLIMA.
#'
#' @param idsesion Character. Identificador de sesión válido proporcionado por \code{getwsIDSesion}.
#'
#' @return data.frame con columnas: \code{PROVINCIA}, \code{NOMBRE}, \code{PREFIJO_TEFN}, \code{COMUNIDAD}.
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' provincias <- getwsProvinciasList(idsesion)
#' head(provincias)
#' }
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @importFrom plyr ldply
#' @export
#'
getwsProvinciasList <- function(idsesion) {
  # Construir la URL SOAP
  url_getProvinciasList <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getProvinciasList&pksesion=", idsesion
  )

  # Descargar el XML como texto (sin escribir a disco)
  xml_text <- call_with_retry(soap_get(url_getProvinciasList),"soap_get(getProvinciasList)")

  # Parsear el XML
  doc <- XML::xmlParse(xml_text)

  # Extraer los nodos de cada campo
  provincia <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//PROVINCIA"), stringsAsFactors = FALSE)
  nombre <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//NOMBRE"), stringsAsFactors = FALSE)
  prefijo_tefn <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//PREFIJO_TEFN"), stringsAsFactors = FALSE)
  comunidad <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//COMUNIDAD"), stringsAsFactors = FALSE)

  # Combinar en un unico data.frame
  df <- data.frame(
    PROVINCIA = unlist(provincia),
    NOMBRE = unlist(nombre),
    PREFIJO_TEFN = unlist(prefijo_tefn),
    COMUNIDAD = unlist(comunidad),
    stringsAsFactors = FALSE
  )

  return(df)
}

