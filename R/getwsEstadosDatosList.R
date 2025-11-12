#' @title Obtener la lista de estados de los datos de la base de datos CLIMA
#'
#' @description
#' Esta función consulta el servicio web CLIMA de la Junta de Andalucía y devuelve un
#' \code{data.frame} con los estados de los datos.
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
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' estados_datos <- getwsEstadosDatosList(idsesion)
#' head(estados_datos)
#' }
#'
#' @importFrom XML xmlParse xmlToDataFrame getNodeSet
#' @importFrom RCurl getURL
#' @importFrom plyr ldply
#' @export
#'
getwsEstadosDatosList <- function(idsesion) {

  # --- Construccion de la URL SOAP ---
  url_getEstadosDatosList <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getEstadosDatosList&pksesion=", idsesion
  )

  # --- Descarga del XML directamente en memoria usando soap_get ---
  xml_text <- call_with_retry(soap_get(url_getEstadosDatosList),"soap_get(getEstadosDatosList)")

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
    CODIGO = unlist(safe_extract("CODIGO")),
    DESCRIPCION = unlist(safe_extract("DESCRIPCION")),
    PKEDD = unlist(safe_extract("PKEDD")),
    stringsAsFactors = FALSE
  )

  # --- Limpieza final ---
  df <- df[rowSums(is.na(df)) < ncol(df), , drop = FALSE]

  message(paste0("Obtenido dataframe de estados de los datos con ",nrow(df)," registros."))

  return(df)
}
