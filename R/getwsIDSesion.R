#' @title Obtener ID de sesión del servicio de clima
#'
#' @description
#' Esta función solicita un identificador de sesión (ID de sesión)
#' al servicio de clima de la Junta de Andalucía, autenticando con usuario y contraseña.
#'
#' @param user Character. Nombre de usuario autorizado.
#' @param password Character. Contraseña correspondiente.
#'
#' @return Character. ID de sesión válido para usar en llamadas SOAP posteriores.
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' print(idsesion)
#' }
#'
#' @importFrom XML xmlParse xmlToDataFrame getNodeSet
#' @export
#'
getwsIDSesion <- function(user, password) {

  url <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getIDSesion&usuario=", user, "&password=", password
  )
  xml_text <- call_with_retry(soap_get(url),"soap_get(getIDSesion)")
  doc <- xmlParse(xml_text)
  idsesion <- xmlToDataFrame(nodes = XML::getNodeSet(doc, "//getIDSesionReturn"), stringsAsFactors = FALSE)

  message("Obtenido identificador de sesi\u00F3n")

  return(idsesion[1, 1])

}
