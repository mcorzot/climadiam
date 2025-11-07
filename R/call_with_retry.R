#' @title Ejecutar una expresión con reintentos automáticos ante error
#'
#' @description
#' Función auxiliar que intenta ejecutar una expresión varias veces en caso de error
#' (por ejemplo, una llamada SOAP o HTTP que falle temporalmente).
#' Si todas las tentativas fallan, la función genera un error y **no devuelve un
#' valor vacío**, garantizando un control de flujo claro en las funciones que la usan.
#'
#' @param expr Expresión o llamada a evaluar (por ejemplo, \code{soap_get(url)}).
#' @param name Character. Nombre identificativo de la operación (usado en los mensajes
#' de advertencia y error para facilitar la depuración).
#' @param retries Integer. Número máximo de intentos (por defecto \code{2}).
#' @param wait Numeric. Tiempo de espera (en segundos) entre intentos (por defecto \code{1}).
#'
#' @return Devuelve el resultado de la expresión si se ejecuta correctamente.
#' Si todos los intentos fallan, se lanza un error con información descriptiva.
#'
#' @details
#' Esta función es útil para funciones que realizan llamadas externas (por ejemplo,
#' servicios SOAP o API REST) que pueden fallar de forma intermitente.
#' Detecta errores usando \code{try()} y vuelve a intentar la ejecución el número de
#' veces especificado por \code{retries}.
#' En caso de éxito, devuelve el resultado original sin modificar.
#'
#' @examples
#' \dontrun{
#' # Ejemplo de uso con una llamada potencialmente inestable:
#' result <- call_with_retry({
#'   soap_get("http://servidor/prueba?param=x")
#' }, name = "soap_get(prueba)", retries = 3, wait = 2)
#' }
#'
#' @export
#' @keywords internal
#'
call_with_retry <- function(expr, name, retries = 3, wait = 2) {
  attempt <- 1
  while (attempt <= retries) {
    result <- try(expr, silent = TRUE)
    if (!inherits(result, "try-error") && !is.null(result) && !identical(result, "")) {
      return(result)
    }
    if (attempt < retries) {
      warning(sprintf(
        "Error al ejecutar '%s' (intento %d/%d). Reintentando en %.1f segundos...",
        name, attempt, retries, wait
      ), call. = FALSE)
      Sys.sleep(wait)
    }
    attempt <- attempt + 1
  }
  stop(sprintf("%s' fall\u00F3 tras %d intentos. Revisa la conexi\u00F3n o los par\u00E1metros.",
               name, retries), call. = FALSE)
}
