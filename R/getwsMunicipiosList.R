#' @title Obtener la lista de municipios de la base de datos del CLIMA
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de Andalucía
#' para obtener la lista de municipios en la base de datos CLIMA.
#'
#' @param idsesion Character. Identificador de sesión válido proporcionado por \code{getwsIDSesion}.
#'
#' @return data.frame con columnas: \code{PROVINCIA}, \code{MUNICIPIO}, \code{NOMBRE}.
#'
#' @examples
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' municipios <- getwsMunicipiosList(idsesion)
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @importFrom plyr ldply
#' @export
#'
getwsMunicipiosList <- function(idsesion) {
  # Construir la URL SOAP
  url_getMunicipiosList <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getMunicipiosList&pksesion=", idsesion
  )

  # Descargar el XML como texto (sin escribir a disco)
  xml_text <- call_with_retry(soap_get(url_getMunicipiosList),"soap_get(getMunicipiosList)")

  # Parsear el XML
  doc <- XML::xmlParse(xml_text)

  # Extraer los nodos de cada campo
  provincia <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//PROVINCIA"), stringsAsFactors = FALSE)
  municipio <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//MUNICIPIO"), stringsAsFactors = FALSE)
  nombre <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//NOMBRE"), stringsAsFactors = FALSE)

  # Combinar en un unico data.frame
  df <- data.frame(
    PROVINCIA = unlist(provincia),
    MUNICIPIO = unlist(municipio),
    NOMBRE = unlist(nombre),
    stringsAsFactors = FALSE
  )

  return(df)
}

