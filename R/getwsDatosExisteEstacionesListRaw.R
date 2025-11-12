#' @title Obtener la lista de estaciones con datos existentes (en bruto) con reintentos
#'
#' @description
#' Consulta el servicio web CLIMA de la Junta de Andalucía mediante el método
#' \code{getDatosExisteEstacionesList}, que devuelve la lista de estaciones con datos
#' existentes según los parámetros de búsqueda.
#' Si se introduce el identificador interno de una estación, devuelve sólo el registro solicitado.
#'
#' Esta versión incluye reintentos automáticos y control de tiempo de espera (timeout)
#' para garantizar la estabilidad ante errores o respuestas vacías del servicio SOAP.
#'
#' @param idsesion Character. Identificador de sesión válido obtenido mediante \code{getwsIDSesion()}.
#' @param pkest Character. Código de estación (opcional, por defecto todas).
#' @param retries Integer. Número máximo de intentos en caso de fallo o resultado vacío (por defecto 3).
#' @param wait Numeric. Tiempo de espera entre reintentos en segundos (por defecto 2).
#' @param timeout_sec Numeric. Tiempo máximo de espera por la respuesta del servicio SOAP en segundos (por defecto 5).
#'
#' @return
#' Un \code{data.frame} con las columnas devueltas por el servicio:
#' \itemize{
#'   \item \code{ESTPKEST} — Identificador interno de la estación.
#'   \item \code{FECFINDATOS} — Fecha final de los datos.
#'   \item \code{FECHACARGA} — Fecha de carga de los datos.
#'   \item \code{FECINICIODATOS} — Fecha inicial de los datos.
#'   \item \code{LEXISTE} — Indicador lógico de existencia de datos.
#'   \item \code{PKDEE} — Identificador interno del registro.
#' }
#' Si tras todos los intentos no se obtiene información, se lanza un error.
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' pkest <- '183' # EARM22
#' existencias_estaciones <- getwsDatosExisteEstacionesListRaw(idsesion, pkest)
#' head(existencias_estaciones)
#' }
#'
#' @importFrom XML xmlParse xmlToDataFrame getNodeSet
#' @importFrom R.utils withTimeout
#' @export
#'
#'
getwsDatosExisteEstacionesListRaw <- function(idsesion,pkest = NULL,retries = 3,wait = 2,timeout_sec = 5) {

  attempt <- 1

  while (attempt <= retries) {
    # --- Construcción de la URL ---
    if (!is.null(pkest)) {
      url_getDatosExisteEstacionesList <- paste0(
        "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
        "?method=getDatosExisteEstacionesList",
        "&PKSESION=", idsesion,
        "&ESTPKEST=", pkest
      )
    } else {
      url_getDatosExisteEstacionesList <- paste0(
        "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
        "?method=getDatosExisteEstacionesList",
        "&PKSESION=", idsesion
      )
    }

    # --- Llamada SOAP con timeout y reintentos automáticos ---
    xml_text <- call_with_retry(
      expr = R.utils::withTimeout(
        soap_get(url_getDatosExisteEstacionesList),
        timeout = timeout_sec,
        onTimeout = "error"
      ),
      name = "soap_get(getDatosExisteEstacionesList)"
    )

    # --- Parseo del XML ---
    doc <- XML::xmlParse(xml_text)

    # --- Función auxiliar de extracción segura ---
    safe_extract <- function(tag) {
      nodes <- XML::getNodeSet(doc, paste0("//", tag))
      if (length(nodes) > 0) {
        return(XML::xmlToDataFrame(nodes = nodes, stringsAsFactors = FALSE))
      } else {
        return(data.frame(V1 = NA_character_, stringsAsFactors = FALSE))
      }
    }

    # --- Extracción de los campos ---
    df <- data.frame(
      ESTPKEST = unlist(safe_extract("ESTPKEST")),
      FECFINDATOS = unlist(safe_extract("FECFINDATOS")),
      FECHACARGA = unlist(safe_extract("FECHACARGA")),
      FECINICIODATOS = unlist(safe_extract("FECINICIODATOS")),
      LEXISTE = unlist(safe_extract("LEXISTE")),
      PKDEE = unlist(safe_extract("PKDEE")),
      stringsAsFactors = FALSE
    )

    # --- Limpieza: eliminar filas vacías ---
    df <- df[rowSums(is.na(df)) < ncol(df), , drop = FALSE]

    if (nrow(df) > 0) {
      message(paste0("Obtenido dataframe de existencias de datos por estaciones en bruto con ", nrow(df), " registros."))
      return(df)
    } else {
      warning(sprintf(
        "No se han encontrado estaciones que cumplan los criterios (intento %d/%d). Reintentando en %d segundos...",
        attempt, retries, wait
      ), call. = FALSE)
      Sys.sleep(wait)
    }

    attempt <- attempt + 1
  }

  stop(sprintf("No se pudo obtener informaci\u00F3n de estaciones tras %d intentos. Revisa la conexi\u00F3n o los par\u00E1metros.", retries))
}
