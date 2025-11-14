#' @title Obtener la lista de estaciones asociadas a una variable con reintentos
#'
#' @description
#' Consulta el servicio web CLIMA de la Junta de Andalucía y devuelve un
#' \code{data.frame} con los identificadores internos de las estaciones que
#' registran datos para una variable determinada.
#' La función reintenta automáticamente si la llamada falla, si el XML es inválido,
#' o si no se encuentra información (`nrow(df) == 0`).
#'
#' @param varpkvar Character. Identificador de la variable (VARPKVAR) según el catálogo del servicio CLIMA.
#' @param idsesion Character. Identificador de sesión válido proporcionado por \code{getwsIDSesion}.
#' @param retries Integer. Número máximo de intentos (por defecto 3).
#' @param wait Numeric. Tiempo de espera entre reintentos en segundos (por defecto 2).
#' @param timeout_sec Numeric. Tiempo máximo de espera por la respuesta del servicio SOAP en segundos (por defecto 5).
#'
#' @return
#' Un \code{data.frame} con las columnas:
#' \itemize{
#'   \item \code{ESTPKEST} — Identificador interno de la estación.
#'   \item \code{EXISTENDATOS} — Indicador de disponibilidad de datos.
#'   \item \code{FACTOR} — Factor de corrección o calibración.
#'   \item \code{LIMITEVAL} — Límite de validación.
#'   \item \code{ORIGEN} — Origen de los datos.
#'   \item \code{PKVPE} — Identificador del vínculo variable-estación.
#'   \item \code{RANGOINFERIOR} — Rango inferior permitido para la variable.
#'   \item \code{RANGOSUPERIOR} — Rango superior permitido para la variable.
#'   \item \code{VARPKVAR} — Identificador interno de la variable asociada.
#' }
#' Si tras todos los intentos no se encuentra información, lanza un error.
#'
#' @examples
#' \dontrun{
#' idsesion <- getwsIDSesion("usuario","password")
#' varpkvar <- '1' # TI1
#' variables_estaciones <- getwsVariablesEstacionesListRaw(varpkvar, idsesion)
#' head(variables_estaciones)
#' }
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @importFrom R.utils withTimeout
#' @importFrom stats runif
#' @export
#'
getwsVariablesEstacionesListRaw <- function(varpkvar, idsesion, retries = 3, wait = 2, timeout_sec = 5) {
  attempt <- 1

  while (attempt <= retries) {
    url <- paste0(
      "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
      "?method=getVariablesEstacionesList&VARPKVAR=", varpkvar,
      "&pksesion=", idsesion
    )

    # Llamada SOAP con timeout y reintentos
    xml_text <- call_with_retry(
      expr = R.utils::withTimeout(
        soap_get(url),
        timeout = timeout_sec,
        onTimeout = "error"
      ),
      name = "soap_get(getVariablesEstacionesList)"
    )

    doc <- XML::xmlParse(xml_text)

    extract_nodes <- function(tag) {
      XML::xmlToDataFrame(nodes = XML::getNodeSet(doc, paste0("//", tag)), stringsAsFactors = FALSE)
    }

    df <- data.frame(
      ESTPKEST = unlist(extract_nodes("ESTPKEST")),
      EXISTENDATOS = unlist(extract_nodes("EXISTENDATOS")),
      FACTOR = unlist(extract_nodes("FACTOR")),
      LIMITEVAL = unlist(extract_nodes("LIMITEVAL")),
      ORIGEN = unlist(extract_nodes("ORIGEN")),
      PKVPE = unlist(extract_nodes("PKVPE")),
      RANGOINFERIOR = unlist(extract_nodes("RANGOINFERIOR")),
      RANGOSUPERIOR = unlist(extract_nodes("RANGOSUPERIOR")),
      VARPKVAR = unlist(extract_nodes("VARPKVAR")),
      stringsAsFactors = FALSE
    )

    if (nrow(df) > 0) {
      message(paste0("Obtenido dataframe de estaciones por variable con ", nrow(df), " registros."))
      return(df)
    } else {
      warning(sprintf(
        "No se encontraron estaciones para la variable '%s' (intento %d/%d). Reintentando en %d segundos...",
        varpkvar, attempt, retries, wait
      ), call. = FALSE)
      Sys.sleep(wait)
      # Pausa aleatoria entre 1 y 3 segundos
      Sys.sleep(runif(1, 1, 3))
    }

    attempt <- attempt + 1
  }

  stop(sprintf("No se pudo obtener informaci\u00F3n de las estaciones para la variable '%s' tras %d intentos.", varpkvar, retries))
}

