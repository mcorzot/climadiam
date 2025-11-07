#' @title Obtener la lista de estaciones con datos existentes (en bruto)
#'
#' @description
#' Consulta el servicio web CLIMA de la Junta de Andalucía mediante el método
#' \code{getDatosExisteEstacionesList}, que devuelve la lista de estaciones con datos
#' existentes según los parámetros de búsqueda. Si se introduce el identificador
#' interno de una estación, devuelve sólo el registro solicitado
#'
#' @param idsesion \code{character}. Identificador de sesión válido obtenido mediante \code{getwsIDSesion()}.
#' @param pkest \code{character}. Código de estación (opcional, por defecto todas).
#'
#' @return
#' Un \code{data.frame} con las columnas devueltas por el servicio (por ejemplo: \code{ESTPKEST}, \code{FECFINDATOS}, \code{FECHACARGA}, \code{FECINICIODATOS}, \code{LEXISTE}, \code{PKDEE}
#'
#' @examples
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' pkest <- '183' # EARM22
#' existencias_estaciones <- getwsDatosExisteEstacionesListRaw(idsesion,pkest)
#' print(existencias_estaciones)
#'
#' @importFrom XML xmlParse xmlToDataFrame getNodeSet
#' @importFrom RCurl getURL
#' @importFrom plyr ldply
#' @export
#'
getwsDatosExisteEstacionesListRaw <- function(idsesion,pkest = NULL) {

  # --- Construccion de la URL del metodo ---
  if (!is.null(pkest)){
    url_getDatosExisteEstacionesList <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getDatosExisteEstacionesList",
    "&PKSESION=", idsesion,
    "&ESTPKEST=", pkest)
    } else {
      url_getDatosExisteEstacionesList <- paste0(
        "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
        "?method=getDatosExisteEstacionesList",
        "&PKSESION=", idsesion)
    }

  # --- Descarga del XML directamente en memoria usando soap_get ---
  xml_text <- call_with_retry(soap_get(url_getDatosExisteEstacionesList),"soap_get(getDatosExisteEstacionesList)")

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
    ESTPKEST = unlist(safe_extract("ESTPKEST")),
    FECFINDATOS = unlist(safe_extract("FECFINDATOS")),
    FECHACARGA = unlist(safe_extract("FECHACARGA")),
    FECINICIODATOS = unlist(safe_extract("FECINICIODATOS")),
    LEXISTE = unlist(safe_extract("LEXISTE")),
    PKDEE = unlist(safe_extract("PKDEE")),
    stringsAsFactors = FALSE
  )

  # --- Limpieza final ---
  df <- df[rowSums(is.na(df)) < ncol(df), , drop = FALSE]

  return(df)
}
