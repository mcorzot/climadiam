#' @title Obtener datos intradiarios de varias estaciones, variables y/o fechas
#' y devolverlos como data.frame (multiconsulta)
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de
#' Andalucía para obtener datos intradiarios de estaciones, variables y
#' fechas y devuelve un data.frame. Permite combinar varias elementos, manteniendo
#' siempre un elemento único de partida, por ejemplo, varias estaciones y fechas
#' para una variable o varias fechas y variables para una estación.
#'
#' @param idsesion Character. Identificador de sesión válido obtenido previamente.
#' @param cestaciones Character. Vector con los identificadores de las estaciones a consultar.
#' @param cvariables Character vector. Vector con los identificadores de las variables a consultar.
#' @param fechas Character vector. Vector con las fechas a consultar en formato DD/MM/AAAA.
#'
#' @return data.frame con columnas: cestacion, cvariable, fecha, hora y valor.
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' cestaciones <- c('EARM22')
#' cvariables <- c('TI1','PI1')
#' fechas <- c('26/10/2025','27/10/2025')
#' datos_intra <- getwsDatosIntradiariosMultiRaw(idsesion,cestaciones,cvariables,fechas)
#' head(datos_intra)
#' }
#'
#' @importFrom RCurl postForm
#' @importFrom XML xmlParse getNodeSet xmlChildren xmlValue
#' @export
#'
getwsDatosIntradiariosMulti <- function(idsesion,cestaciones,cvariables,fechas) {

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

  # Se consultan los datos raw del día objetivo y el previo
  datos_df <- data.frame()
  for (l in 1:length(pkfecs)){
    df <- data.frame()
    df_prev <- data.frame()
    pkfec <- pkfecs[l]
    pkfec_prev <- as.character(as.integer(pkfec)-1)
    df_prev <- getwsDatosIntradiariosMultiRaw(idsesion,pkests,pkvars,pkfec_prev)
    df <- getwsDatosIntradiariosMultiRaw(idsesion,pkests,pkvars,pkfec)
    df <- rbind(df_prev,df)
    datos_df <- rbind(datos_df,df)
  }

  # Se relaciona con las horas-minutos
  datos_df <- merge(x = datos_df, y = minutos_horas, by.x='minuto', by.y='MINUTOS')

  # Se convierte el dato de las 24:00 en el de las 00:00 del dia siguiente
  datos_df$HORA[datos_df$HORA=="24:00"] <- "00:00"
  datos_df$pkfec <- ifelse(datos_df$HORA %in% "00:00", as.character(as.integer(datos_df$pkfec)+1), datos_df$pkfec)

  # Se filtra teniendo en cuenta la fecha indicada en la consulta
  datos_df <- datos_df[datos_df$pkfec %in% pkfecs,]

  # Se borran duplicados
  datos_df <- datos_df[!duplicated(datos_df), ]

  # Se ordena el dataframe por hora
  datos_df <- datos_df[order(datos_df$pkest,datos_df$pkvar,datos_df$pkfec,datos_df$HORA),]

  # Se asocian los identificadores internos con los valores aportados en la funcion
  datos_df <- merge(x = datos_df, y = estaciones_df, by.x = 'pkest', by.y = 'PKEST')
  datos_df <- merge(x = datos_df, y = variables_df, by.x = 'pkvar', by.y = 'PKVAR')
  datos_df <- merge(x = datos_df, y = fechas_df, by.x = 'pkfec', by.y = 'PKFEC')

  # Se compone el dataframe
  df <- data.frame(datos_df$CESTACION,datos_df$CVARIABLE,datos_df$FECHA,datos_df$HORA,datos_df$valor)
  colnames(df) <- c('cestacion','cvariable','fecha','hora','valor')

  return(df)

}
