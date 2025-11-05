#' @title Obtener lista detallada de estaciones meteorológicas del servicio CLIMA
#'
#' @description
#' Esta función consulta el servicio web CLIMA de la Junta de Andalucía
#' y devuelve un \code{data.frame} con información detallada de todas las
#' estaciones meteorológicas disponibles en la base de datos.
#'
#' @param idsesion \code{character}. Identificador de sesión válido obtenido mediante
#' \code{getwsIDSesion()}.
#'
#' @return
#' Un \code{data.frame} con los principales atributos de cada estación, incluyendo
#' coordenadas, red, municipio, provincia, entorno y otros metadatos.
#'
#' @examples
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' estaciones_detallado <- getwsEstacionesExpandidoList(idsesion)
#' head(estaciones_detallado)
#'
#' @importFrom XML xmlParse xmlToDataFrame getNodeSet
#' @importFrom RCurl getURL
#' @importFrom plyr ldply
#' @export
#'
getwsEstacionesExpandidoList <- function(idsesion) {

  # --- Construccion de la URL ---
  url_getEstacionesExpandidoList <- paste0(
    "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima",
    "?method=getEstacionesExpandidoList&pksesion=", idsesion
  )

  # --- Descarga del XML directamente en memoria usando soap_get ---
  xml_text <- soap_get(url_getEstacionesExpandidoList)

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

  # --- Extraccion de todos los campos relevantes ---
  df <- data.frame(
    ACLPKACL = unlist(safe_extract("ACLPKACL")),
    ACLPKACL_COD = unlist(safe_extract("ACLPKACL_COD")),
    ACLPKACL_DEN = unlist(safe_extract("ACLPKACL_DEN")),
    AGEPKAGE = unlist(safe_extract("AGEPKAGE")),
    AGEPKAGE_COD = unlist(safe_extract("AGEPKAGE_COD")),
    AGEPKAGE_DEN = unlist(safe_extract("AGEPKAGE_DEN")),
    CESTACION = unlist(safe_extract("CESTACION")),
    COMPKCOM = unlist(safe_extract("COMPKCOM")),
    COMPKCOM_COD = unlist(safe_extract("COMPKCOM_COD")),
    COMPKCOM_DEN = unlist(safe_extract("COMPKCOM_DEN")),
    COORDENADAX = unlist(safe_extract("COORDENADAX")),
    COORDENADAY = unlist(safe_extract("COORDENADAY")),
    COORDENADAZ = unlist(safe_extract("COORDENADAZ")),
    DENOMINACION = unlist(safe_extract("DENOMINACION")),
    DEXISTE = unlist(safe_extract("DEXISTE")),
    ENTORNO = unlist(safe_extract("ENTORNO")),
    ESTAPKESTA = unlist(safe_extract("ESTAPKESTA")),
    ESTAPKESTA_DEN = unlist(safe_extract("ESTAPKESTA_DEN")),
    FECHAHORAPRO = unlist(safe_extract("FECHAHORAPRO")),
    FECHAHORAULTAGRE = unlist(safe_extract("FECHAHORAULTAGRE")),
    GESPKGES = unlist(safe_extract("GESPKGES")),
    GESPKGES_COD = unlist(safe_extract("GESPKGES_COD")),
    GESPKGES_DEN = unlist(safe_extract("GESPKGES_DEN")),
    GRAFUBICACION = unlist(safe_extract("GRAFUBICACION")),
    LATITUD = unlist(safe_extract("LATITUD")),
    LONGITUD = unlist(safe_extract("LONGITUD")),
    MODELO = unlist(safe_extract("MODELO")),
    MUNMUNICIPIO = unlist(safe_extract("MUNMUNICIPIO")),
    MUNMUNICIPIO_NOM = unlist(safe_extract("MUNMUNICIPIO_NOM")),
    MUNPROVINCIA = unlist(safe_extract("MUNPROVINCIA")),
    MUNPROVINCIA_NOM = unlist(safe_extract("MUNPROVINCIA_NOM")),
    OBSERVACION = unlist(safe_extract("OBSERVACION")),
    OBSERVACIONADM = unlist(safe_extract("OBSERVACIONADM")),
    PKEST = unlist(safe_extract("PKEST")),
    PLUVIO = unlist(safe_extract("PLUVIO")),
    REDPKRED = unlist(safe_extract("REDPKRED")),
    REDPKRED_COD = unlist(safe_extract("REDPKRED_COD")),
    REDPKRED_DEN = unlist(safe_extract("REDPKRED_DEN")),
    WEB = unlist(safe_extract("WEB")),
    stringsAsFactors = FALSE
  )

  # --- Limpieza: eliminar filas completamente vacias ---
  df <- df[rowSums(is.na(df)) < ncol(df), , drop = FALSE]

  return(df)
}
