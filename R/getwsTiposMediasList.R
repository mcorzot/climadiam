#' @title Obtener la lista de tipos de medias (intervalos de agregación) en la base de datos CLIMA
#'
#' @description
#' Esta función consulta el servicio web CLIMA de la Junta de Andalucía y devuelve un
#' \code{data.frame} con los tipos de medias disponibles (por ejemplo: diaria, horaria, mensual).
#'
#' @param idsesion \code{character}. Identificador de sesión válido obtenido mediante
#' \code{getwsIDSesion()}.
#'
#' @return
#' Un \code{data.frame} con las columnas:
#' \itemize{
#'   \item \code{CTME} — Código del tipo de media.
#'   \item \code{DENOMINACION} — Nombre descriptivo del tipo de media.
#'   \item \code{PERIODO} — Periodo temporal de la media (por ejemplo, 24h).
#'   \item \code{PKTME} — Identificador único del tipo de media.
#' }
#'
#' @examples
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' tipos_medias <- getwsTiposMediasList(idsesion)
#' head(tipos_medias)
#'
#' @importFrom XML xmlParse xmlToDataFrame getNodeSet
#' @importFrom RCurl getURL
#' @importFrom plyr ldply
#' @export
#'
getwsTiposMediasList <- function(idsesion) {

  # --- Construccion de la URL SOAP ---
  url_getTiposMediasList <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getTiposMediasList&pksesion=", idsesion
  )

  # --- Descarga del XML directamente en memoria usando soap_get ---
  xml_text <- call_with_retry(soap_get(url_getTiposMediasList),"soap_get(getTiposMediasList)")

  # --- Parseo del XML ---
  doc <- xmlParse(xml_text)

  # --- Funcion interna para extraer campos con seguridad ---
  safe_extract <- function(tag) {
    nodes <- XML::getNodeSet(doc, paste0("//", tag))
    if (length(nodes) > 0) {
      return(xmlToDataFrame(nodes = nodes, stringsAsFactors = FALSE))
    } else {
      return(data.frame(V1 = NA_character_, stringsAsFactors = FALSE))
    }
  }

  # --- Extraccion de campos ---
  df <- data.frame(
    CTME = unlist(safe_extract("CTME")),
    DENOMINACION = unlist(safe_extract("DENOMINACION")),
    PERIODO = unlist(safe_extract("PERIODO")),
    PKTME = unlist(safe_extract("PKTME")),
    stringsAsFactors = FALSE
  )

  # --- Limpieza final ---
  df <- df[rowSums(is.na(df)) < ncol(df), , drop = FALSE]

  return(df)
}
