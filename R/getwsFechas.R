#' @title Obtener el identificador de una fecha de la base de datos CLIMA con reintentos
#'
#' @description
#' Recupera información de una fecha meteorológica desde el servicio SOAP de la Junta de Andalucía.
#' La función reintenta automáticamente si la llamada falla, si el XML es inválido,
#' o si no se encuentra información (`nrow(df) == 0`).
#'
#' @param date Character. Fecha en formato "DD/MM/YYYY".
#' @param idsesion Character. Identificador de sesión válido para el servicio SOAP.
#' @param retries Integer. Número máximo de intentos (por defecto 3).
#' @param wait Numeric. Tiempo de espera entre reintentos en segundos (por defecto 2).
#' @param timeout_sec Numeric. Tiempo máximo de espera por la respuesta del servicio SOAP en segundos (por defecto 5).
#'
#' @return
#' Devuelve un `data.frame` con los siguientes campos:
#' \itemize{
#'   \item PKFEC
#'   \item FECHA
#' }
#' Si tras todos los intentos no se encuentra información, lanza un error.
#'
#' @details
#' La función utiliza `call_with_retry` para manejar errores de conexión,
#' XML inválido y SOAP Faults. También reintenta automáticamente si el resultado está vacío.
#'
#' @examples
#' \dontrun{
#' idsesion <- getwsIDSesion("usuario", "password")
#' fecha <- getwsFechas("20/10/2025", idsesion)
#' head(fecha)
#' }
#'
#' @importFrom XML xmlParse xmlToDataFrame getNodeSet
#' @importFrom R.utils withTimeout
#' @importFrom stats runif
#' @export
#'
getwsFechas <- function(date, idsesion, retries = 5, wait = 3, timeout_sec = 5) {
  attempt <- 1

  while (attempt <= retries) {
    url <- paste0(
      "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
      "?method=getFechas&pksesion=", idsesion, "&FECHA=", date
    )

    # Llamada SOAP con timeout y reintentos
    xml_text <- call_with_retry(
      expr = R.utils::withTimeout(
        soap_get(url),
        timeout = timeout_sec,
        onTimeout = "error"
      ),
      name = "soap_get(getFechas)"
    )

    doc <- XML::xmlParse(xml_text)

    pkfec <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//PKFEC"), stringsAsFactors = FALSE)
    fecha <- XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, "//FECHA"), stringsAsFactors = FALSE)

    df <- data.frame(PKFEC = unlist(pkfec), FECHA = unlist(fecha), stringsAsFactors = FALSE)

    if (nrow(df) > 0) {
      message(paste0("Obtenido dataframe de fechas con ", nrow(df), " registros."))
      return(df)
    } else {
      warning(sprintf(
        "No se encontr\u00F3 la fecha '%s' (intento %d/%d). Reintentando en %d segundos...",
        date, attempt, retries, wait
      ), call. = FALSE)
      Sys.sleep(wait)
      # Pausa aleatoria entre 1 y 3 segundos
      Sys.sleep(runif(1, 1, 3))
    }

    attempt <- attempt + 1
  }

  stop(sprintf("No se pudo obtener informaci\u00F3n de la fecha '%s' tras %d intentos.", date, retries))
}
