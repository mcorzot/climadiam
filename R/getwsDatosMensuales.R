#' @title Obtener datos mensuales a partir de los identificadores de estaciones,
#' variables y fechas y devolverlos como data.frame
#'
#' @description Esta función realiza una llamada SOAP al servicio de clima de la
#' Junta de Andalucía para obtener datos mensuales de varias estaciones, variables
#' y fechas, y devolverlos como data.frame.
#'
#' @param idsesion Character. Identificador de sesión válido obtenido previamente.
#' @param cestaciones Character vector. Vector con los identificadores de las estaciones.
#' @param cvariables Character vector. Vector con los identificadores de las variables.
#' @param fechas Character vector. Vector con las fechas del último día de cada mes en formato DD/MM/AAAA.
#'
#' @return data.frame con columnas: cestacion, cvariable, fecha, valor.
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' cestaciones <- c('EARM22','EARM01')
#' cvariables <- c('TM1')
#' fechas <- c('31/12/2020')
#' datos_mensuales <- getwsDatosMensuales(idsesion,cestaciones,cvariables,fechas)
#' head(datos_mensuales)
#' }
#'
#' @importFrom RCurl postForm
#' @importFrom XML xmlParse getNodeSet xmlChildren xmlValue xmlSize
#' @export
#'
getwsDatosMensuales <- function(idsesion,cestaciones,cvariables,fechas) {

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
  datos_df <- getwsDatosMensualesRaw(idsesion,pkests,pkvars,pkfecs)

  # En caso de que la consulta no obtenga registros se genera dataframe vacio
  if (nrow(datos_df)==0){
    df <- data.frame(
      cestacion = character(),
      cvariable = character(),
      fecha = character(),
      valor = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    # Se asocian los identificadores internos con los valores aportados en la funcion
    datos_df <- merge(x = datos_df, y = estaciones_df, by.x = 'pkest', by.y = 'PKEST')
    datos_df <- merge(x = datos_df, y = variables_df, by.x = 'pkvar', by.y = 'PKVAR')
    datos_df <- merge(x = datos_df, y = fechas_df, by.x = 'pkfec', by.y = 'PKFEC')

    # Se compone el dataframe
    df <- data.frame(datos_df$CESTACION,datos_df$CVARIABLE,datos_df$FECHA,datos_df$valor)
    colnames(df) <- c('cestacion','cvariable','fecha','valor')
  }

  message(paste0("Obtenido dataframe de datos mensuales con ",nrow(df)," registros."))

  return(df)
}

