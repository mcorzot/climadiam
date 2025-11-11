#' @title Obtener la lista de estaciones asociadas a una variable a partir del
#' identificador interno de la variable y devolverla como data.frame
#'
#' @description
#' Consulta el servicio web CLIMA de la Junta de Andalucía y devuelve un
#' \code{data.frame} con los identificadores internos de las estaciones que
#' registran datos para una variable determinada.
#'
#' @param varpkvar \code{character}. Identificador de la variable (VARPKVAR)
#'   según el catálogo del servicio CLIMA.
#' @param idsesion \code{character}. Identificador de sesión válido proporcionado por
#'   el servicio \code{getwsIDSesion()}.
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
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' varpkvar <- '1' # TI1
#' variables_estaciones <- getwsVariablesEstacionesListRaw(varpkvar, idsesion)
#' head(variables_estaciones)
#'}
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @export
#'
getwsVariablesEstacionesListRaw <- function(varpkvar, idsesion) {

  url <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getVariablesEstacionesList&VARPKVAR=", varpkvar,
    "&pksesion=", idsesion
  )

  xml_text <- call_with_retry(soap_get(url),"soap_get(getVariablesEstacionesList)")
  doc <- xmlParse(xml_text)

  extract_nodes <- function(tag) {
    xmlToDataFrame(nodes = XML::getNodeSet(doc, paste0("//", tag)), stringsAsFactors = FALSE)
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

  return(df)
}
