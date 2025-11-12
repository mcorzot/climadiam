#' @title Obtener información de estaciones con reintentos automáticos
#'
#' @description
#' Recupera información de una estación meteorológica desde el servicio SOAP de la Junta de Andalucía.
#' La función reintenta automáticamente si la llamada falla, si el XML es inválido,
#' o si no se encuentra información (`nrow(df) == 0`).
#'
#' @param cestacion Character. Código de la estación a consultar.
#' @param idsesion Character. Identificador de sesión válido para el servicio SOAP.
#' @param retries Integer. Número máximo de intentos (por defecto 3).
#' @param wait Numeric. Tiempo de espera entre reintentos en segundos (por defecto 2).
#' @param timeout_sec Numeric. Tiempo máximo de espera por la respuesta del servicio SOAP en segundos (por defecto 5).
#'
#' @return
#' Devuelve un `data.frame` con los campos de la estación:
#' \itemize{
#'   \item ACLPKACL
#'   \item AGEPKAGE
#'   \item CESTACION
#'   \item COMPKCOM
#'   \item COORDENADAX
#'   \item COORDENADAY
#'   \item COORDENADAZ
#'   \item DENOMINACION
#'   \item ESTAPKESTA
#'   \item GESPKGES
#'   \item LATITUD
#'   \item LONGITUD
#'   \item MODELO
#'   \item MUNMUNICIPIO
#'   \item MUNPROVINCIA
#'   \item PKEST
#'   \item REDPKRED
#' }
#' Si tras todos los intentos no se encuentra información, lanza un error.
#'
#' @details
#' La función utiliza `call_with_retry` para manejar errores de conexión,
#' XML inválido y SOAP Faults. También reintenta automáticamente si el resultado está vacío.
#'
#' @examples
#' \dontrun{
#' df <- getwsEstaciones("1234", "abc123")
#' }
#'
#' @importFrom XML xmlParse xmlToDataFrame getNodeSet
#' @importFrom R.utils withTimeout
#' @export
getwsEstaciones <- function(cestacion, idsesion, retries = 3, wait = 2, timeout_sec = 5) {
  attempt <- 1

  while (attempt <= retries) {
    url <- paste0(
      "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
      "?method=getEstaciones&CESTACION=", cestacion, "&pksesion=", idsesion
    )

    # Llamada SOAP con timeout y reintentos
    xml_text <- call_with_retry(
      expr = R.utils::withTimeout(
        soap_get(url),
        timeout = timeout_sec,
        onTimeout = "error"
      ),
      name = "soap_get(getEstaciones)"
    )

    doc <- XML::xmlParse(xml_text)

    extract_nodes <- function(tag) {
      XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, paste0("//", tag)), stringsAsFactors = FALSE)
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

    if (nrow(df) > 0) {
      message(paste0("Obtenido dataframe de estaciones con ", nrow(df), " registros."))
      return(df)
    } else {
      warning(sprintf(
        "No se encontr\u00F3 la estaci\u00F3n '%s' (intento %d/%d). Reintentando en %d segundos...",
        cestacion, attempt, retries, wait
      ), call. = FALSE)
      Sys.sleep(wait)
    }

    attempt <- attempt + 1
  }

  stop(sprintf("No se pudo obtener informaci\u00F3n de la estaci\u00F3n '%s' tras %d intentos.", cestacion, retries))
}
