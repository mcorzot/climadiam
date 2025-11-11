#' @title Obtener la lista de estaciones asociadas a una variable a partir del
#' código de la variable y devolverla como data.frame
#'
#' @description
#' Consulta el servicio web CLIMA de la Junta de Andalucía y devuelve un
#' \code{data.frame} con los códigos de las estaciones que registran datos para
#' una variable determinada.
#'
#' @param cvariable \code{character}. Identificador de la variable (CVARIABLE)
#'   según el catálogo del servicio CLIMA.
#' @param idsesion \code{character}. Identificador de sesión válido proporcionado por
#'   el servicio \code{getwsIDSesion()}.
#'
#' @return
#' Un \code{data.frame} con las columnas:
#' \itemize{
#'   \item \code{ESTPKEST} — Identificador interno de la estación.
#'   \item \code{CESTACION} — Identificador de la estación.
#'   \item \code{EXISTENDATOS} — Indicador de disponibilidad de datos.
#'   \item \code{FACTOR} — Factor de corrección o calibración.
#'   \item \code{LIMITEVAL} — Límite de validación.
#'   \item \code{ORIGEN} — Origen de los datos.
#'   \item \code{PKVPE} — Identificador interno del vínculo variable-estación.
#'   \item \code{RANGOINFERIOR} — Rango inferior permitido para la variable.
#'   \item \code{RANGOSUPERIOR} — Rango superior permitido para la variable.
#'   \item \code{VARPKVAR} — Identificador interno de la variable
#'   \item \code{CVARIABLE} — Identificador de la variable.
#' }
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' cvariable <- 'TI1'
#' variables_estaciones <- getwsVariablesEstacionesList(cvariable, idsesion)
#' head(variables_estaciones)
#'}
#'
#' @importFrom XML xmlParse getNodeSet xmlToDataFrame
#' @export
#'
getwsVariablesEstacionesList <- function(cvariable, idsesion) {

  # Se obtiene la informacion de la variable concreta
  variable_df <- getwsVariables(cvariable,idsesion)
  varpkvar <- variable_df$PKVAR

  # Se obtiene la lista de estaciones
  estaciones_df <- getwsEstacionesList(idsesion)

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

  # Union de dataframes
  df <- merge(x = df, y = estaciones_df, by.x = 'ESTPKEST', by.y = 'PKEST')
  df <- merge(x = df, y = variable_df, by.x = 'VARPKVAR', by.y = 'PKVAR')
  # Seleccion de columnas
  df <- df[,c('ESTPKEST','CESTACION','EXISTENDATOS','FACTOR','LIMITEVAL',
              'ORIGEN','PKVPE','RANGOINFERIOR','RANGOSUPERIOR','VARPKVAR','CVARIABLE')]

  return(df)
}
