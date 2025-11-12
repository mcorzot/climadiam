#' @title Obtener datos diarios de una estación, variable y fecha y devolverlos
#' como data.frame
#'
#' @description Esta función realiza una llamada SOAP al servicio de clima de la
#' Junta de Andalucía para obtener un dato diario de una estación, variable y
#' fecha, y devuelve un data.frame.
#'
#' @param idsesion Character. Identificador de sesión válido obtenido previamente.
#' @param cestacion Character. Identificador de la estación.
#' @param cvariable Character. Identificador de la variable.
#' @param fecha Character. Fecha en formato DD/MM/AAAA.
#'
#' @return data.frame con columnas: cestacion, cvariable, fecha, valor.
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' cestacion <- 'EARM22'
#' cvariable <- 'TD1'
#' fecha <- '26/10/2025'
#' result_diaria <- getwsDatosDiarios(idsesion,cestacion,cvariable,fecha)
#' print(result_diaria)
#' }
#'
#' @importFrom RCurl postForm
#' @importFrom XML xmlParse getNodeSet xmlChildren xmlValue xmlSize
#' @export
#'
getwsDatosDiarios <- function(idsesion,cestacion,cvariable,fecha) {

  # Se obtiene pkest de cestacion
  estacion <- getwsEstaciones(cestacion,idsesion)
  pkest <- estacion$PKEST

  # Se obtiene pkvar de cvariable
  variable <- getwsVariables(cvariable,idsesion)
  pkvar <- variable$PKVAR

  # Se obtiene pkfec de fecha
  fecha_df <- getwsFechas(fecha,idsesion)
  pkfec <- fecha_df$PKFEC

  # Se obtienen los datos en raw
  df <- getwsDatosDiariosRaw(idsesion, pkest, pkvar, pkfec)

  # En caso de que la consulta no obtenga registros se genera dataframe vacio
  if (nrow(df)==0){
    df <- data.frame(
      cestacion = character(),
      cvariable = character(),
      fecha = character(),
      valor = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    # Se compone el dataframe
    df <- data.frame(cestacion,cvariable,fecha,df$valor)
    colnames(df) <- c('cestacion','cvariable','fecha','valor')
  }

  message(paste0("Obtenido dataframe de datos diarios con ",nrow(df)," registros."))

  return(df)
}

