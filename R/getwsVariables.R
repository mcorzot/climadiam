#' @title Obtener una variable meteorológica de la base de datos CLIMA con reintentos
#'
#' @description
#' Recupera información de una variable meteorológica desde el servicio SOAP de la Junta de Andalucía.
#' La función reintenta automáticamente si la llamada falla, si el XML es inválido,
#' o si no se encuentra información (`nrow(df) == 0`).
#'
#' @param cvariable Character. Identificador de la variable a consultar.
#' @param idsesion Character. Identificador de sesión válido para el servicio SOAP.
#' @param retries Integer. Número máximo de intentos (por defecto 3).
#' @param wait Numeric. Tiempo de espera entre reintentos en segundos (por defecto 2).
#' @param timeout_sec Numeric. Tiempo máximo de espera por la respuesta del servicio SOAP en segundos (por defecto 5).
#'
#' @return
#' Devuelve un `data.frame` con los siguientes campos:
#' \itemize{
#'   \item CVARIABLE
#'   \item DENOMINACION
#'   \item FRECUENCIA
#'   \item PKVAR
#'   \item UDMPKUDM
#' }
#' Si tras todos los intentos no se encuentra información, lanza un error.
#'
#' @details
#' La función utiliza `call_with_retry` para manejar errores de conexión,
#' XML inválido y SOAP Faults. También reintenta automáticamente si el resultado está vacío.
#'
#' @examples
#' \dontrun{
#' idsesion <- getwsIDSesion("usuario","password")
#' variable <- getwsVariables("TI1", idsesion)
#' head(variable)
#' }
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @importFrom R.utils withTimeout
#' @export
#'
getwsVariables <- function(cvariable, idsesion, retries = 3, wait = 2, timeout_sec = 5) {
  attempt <- 1

  while (attempt <= retries) {
    url <- paste0(
      "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
      "?method=getVariables&pksesion=", idsesion, "&CVARIABLE=", cvariable
    )

    # Llamada SOAP con timeout y reintentos
    xml_text <- call_with_retry(
      expr = R.utils::withTimeout(
        soap_get(url),
        timeout = timeout_sec,
        onTimeout = "error"
      ),
      name = "soap_get(getVariables)"
    )

    doc <- XML::xmlParse(xml_text)

    extract_nodes <- function(tag) {
      XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, paste0("//", tag)), stringsAsFactors = FALSE)
    }

    df <- data.frame(
      CVARIABLE = unlist(extract_nodes("CVARIABLE")),
      DENOMINACION = unlist(extract_nodes("DENOMINACION")),
      FRECUENCIA = unlist(extract_nodes("FRECUENCIA")),
      PKVAR = unlist(extract_nodes("PKVAR")),
      UDMPKUDM = unlist(extract_nodes("UDMPKUDM")),
      stringsAsFactors = FALSE
    )

    if (nrow(df) > 0) {
      message(paste0("Obtenido dataframe de variables con ", nrow(df), " registros."))
      return(df)
    } else {
      warning(sprintf(
        "No se encontr\u00F3 la variable '%s' (intento %d/%d). Reintentando en %d segundos...",
        cvariable, attempt, retries, wait
      ), call. = FALSE)
      Sys.sleep(wait)
    }

    attempt <- attempt + 1
  }

  stop(sprintf("No se pudo obtener informaci\u00F3n de la variable '%s' tras %d intentos.", cvariable, retries))
}

