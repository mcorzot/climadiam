#' @title Obtener la lista de estaciones con datos existentes (interpretado)
#'
#' @description
#' Consulta el servicio web CLIMA de la Junta de Andalucía mediante el método
#' \code{getDatosExisteEstacionesList}, que devuelve la lista de estaciones con datos
#' existentes según los parámetros de búsqueda. Si se introduce el código de la
#' estación, devuelve sólo el registro solicitado.
#'
#' @param idsesion \code{character}. Identificador de sesión válido obtenido mediante \code{getwsIDSesion()}.
#' @param cestacion \code{character}. Código de estación (opcional, por defecto todas).
#'
#' @return
#' Un \code{data.frame} con las columnas devueltas por el servicio (por ejemplo: \code{ESTPKEST}, \code{FECFINDATOS}, \code{FECHACARGA}, \code{FECINICIODATOS}, \code{LEXISTE}, \code{PKDEE}
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' cestacion <- 'EARM22'
#' existencias_estaciones <- getwsDatosExisteEstacionesList(idsesion,cestacion)
#' print(existencias_estaciones)
#' }
#'
#' @importFrom XML xmlParse xmlToDataFrame getNodeSet
#' @importFrom RCurl getURL
#' @importFrom plyr ldply
#' @importFrom stats runif
#' @export
#'
getwsDatosExisteEstacionesList <- function(idsesion,cestacion = NULL) {

  # Pausa aleatoria entre 1 y 3 segundos
  Sys.sleep(runif(1, 1, 3))

  if(is.null(cestacion)){
    # Si cestacion es NULL, pkest es NULL
    pkest = NULL
    # Se obtiene la lista completa de estaciones
    estacion_df <- getwsEstacionesList(idsesion)
    df <- getwsDatosExisteEstacionesListRaw(idsesion,pkest)
    df <- merge(x = df, y = estacion_df, by.x = 'ESTPKEST', by.y = 'PKEST', all.x = TRUE)
    df <- df[!is.na(df$CESTACION), ]
    df <- df[,c("ESTPKEST","FECFINDATOS","FECHACARGA","FECINICIODATOS","LEXISTE","PKDEE","CESTACION")]

  } else {

    # Se obtiene el pk de cestacion
    estacion_df <- getwsEstaciones(cestacion,idsesion)
    pkest <- estacion_df$PKEST
    # Se obtiene el pk de cestacion
    df <- getwsDatosExisteEstacionesListRaw(idsesion,pkest)
    df$CESTACION <- cestacion
  }

  message(paste0("Obtenido dataframe de existencias de datos por estaci\u00F3n con ",nrow(df)," registros."))

  return(df)
}
