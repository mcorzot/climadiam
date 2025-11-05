#' @title Obtener datos diarios a partir de los identificadores de las
#' estaciones, variables y fechas y devolverlos como data.frame (multiconsulta)
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de
#' Andalucía para obtener datos diarios de varias estaciones, variables y fechas,
#' a partir de sus identificadores, devolviendo un data.frame
#'
#' @param idsesion Character. Identificador de sesión válido obtenido previamente.
#' @param cestaciones Character vector. Identificadores de las estaciones.
#' @param cvariables Character vector. Identificadores de las variables.
#' @param fechas Character vector. Fechas en formato DD/MM/AAAA.
#'
#' @return data.frame con columnas: cestacion, vbariable, fecha y valor
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' cestaciones <- 'EARM22'
#' cvariables <- c('TD1','PD23')
#' fechas <- c('26/10/2025','27/10/2025')
#' result_diaria <- getwsDatosDiariosMulti(idsesion,pkests,pkvars,pkfecs)
#' print(result_diaria)
#' }
#'
#' @importFrom RCurl postForm
#' @importFrom XML xmlParse getNodeSet xmlChildren xmlValue
#' @export
#'
getwsDatosDiariosMulti <- function(idsesion,cestaciones,cvariables,fechas) {

  # Se obtienen los pkests de cestaciones
  estaciones_df <- data.frame()
  for (i in 1:length(cestaciones)){
    estacion <- cestaciones[i]
    estaciones_df_row <- getwsEstaciones(estacion,idsesion)
    estaciones_df <- rbind(estaciones_df,estaciones_df_row)
  }
  pkests <- estaciones_df$PKEST

  # Se obtienen los pkvars de cvariables
  variables_df <- data.frame()
  for (j in 1:length(cvariables)){
    variable <- cvariables[j]
    variables_df_row <- getwsVariables(variable,idsesion)
    variables_df <- rbind(variables_df,variables_df_row)
  }
  pkvars <- variables_df$PKVAR

  # Se obtienen pkfecs de fechas
  fechas_df <- data.frame()
  for (k in 1:length(fechas)){
    fecha <- fechas[k]
    fechas_df_row <- getwsFechas(fecha,idsesion)
    fechas_df <- rbind(fechas_df,fechas_df_row)
  }
  pkfecs <- fechas_df$PKFEC

  # Se consultan los datos en brutos para los criterios indicados
  datos_df <- getwsDatosDiariosMultiRaw(idsesion,pkests,pkvars,pkfecs)

  # Se asocian los identificadores internos con los valores aportados en la funcion
  datos_df <- merge(x = datos_df, y = estaciones_df, by.x = 'pkest', by.y = 'PKEST')
  datos_df <- merge(x = datos_df, y = variables_df, by.x = 'pkvar', by.y = 'PKVAR')
  datos_df <- merge(x = datos_df, y = fechas_df, by.x = 'pkfec', by.y = 'PKFEC')

  # Se compone el dataframe
  df <- data.frame(datos_df$CESTACION,datos_df$CVARIABLE,datos_df$FECHA,datos_df$valor)
  colnames(df) <- c('cestacion','cvariable','fecha','valor')

  return(df)
}
