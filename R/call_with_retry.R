#' @title Ejecutar una expresión con reintentos automáticos
#'
#' @description
#' Evalúa una expresión (como una llamada a un servicio web) con un número limitado
#' de reintentos en caso de error, respuesta vacía o XML no válido.
#'
#' @param expr Expresión a evaluar (por ejemplo, una llamada a una función que devuelve XML).
#' @param name Character. Nombre descriptivo de la operación (usado en los mensajes de error y advertencia).
#' @param retries Integer. Número máximo de intentos (por defecto 3).
#' @param wait Numeric. Tiempo de espera entre reintentos en segundos (por defecto 2).
#'
#' @return
#' Devuelve el resultado de la expresión si es exitoso y contiene XML válido.
#' Si todos los intentos fallan, lanza un error.
#'
#' @details
#' La función comprueba:
#' 1. Que la expresión no produzca un error (`try-error`).
#' 2. Que el resultado no sea `NULL` ni cadena vacía.
#' 3. Que el texto sea un XML bien formado (verificado con `XML::xmlParse`).
#'
#' Si alguna de estas comprobaciones falla, reintenta hasta `retries` veces.
#'
#' @examples
#' \dontrun{
#' xml_result <- call_with_retry(soap_get(url), "soap_get(getEstaciones)")
#' }
#'
#' @importFrom XML xmlParse
#' @export
#'
call_with_retry <- function(expr, name, retries = 5, wait = 3) {
  attempt <- 1
  while (attempt <= retries) {
    result <- try(expr, silent = TRUE)

    # Validar que la llamada no de error y tenga contenido
    if (!inherits(result, "try-error") && !is.null(result) && nzchar(result)) {
      # Intentar parsear como XML para comprobar validez
      valid_xml <- tryCatch({
        XML::xmlParse(result)
        TRUE
      }, error = function(e) FALSE)

      if (valid_xml) {
        return(result)
      }
    }

    # Si llega aqui, el intento ha fallado
    if (attempt < retries) {
      warning(sprintf(
        "Error al ejecutar '%s' (intento %d/%d). Reintentando en %.1f segundos...",
        name, attempt, retries, wait
      ), call. = FALSE)
      Sys.sleep(wait)
    }

    attempt <- attempt + 1
  }

  stop(sprintf("'%s' fall\u00F3 tras %d intentos. Revisa la conexi\u00F3n o los par\u00E1metros.",
               name, retries), call. = FALSE)
}

