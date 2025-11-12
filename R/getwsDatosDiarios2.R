#' @title Obtener datos diarios de un periodo a partir de los identificadores
#' de estaciones, variables y fechas, y devolverlos como data.frame
#'
#' @description
#' Llama al método \code{getDatosDiarios2} del servicio CLIMA de la Junta de
#' Andalucía para obtener datos diarios de varias estaciones y variables entre
#' dos fechas. Las fechas se generan secuencialmente entre \code{fecha_ini} y
#' \code{fecha_fin} por lo que, en caso de lagunas, pueden no ser correctos.
#'
#' Se recomienda, en caso de que haya lagunas en la serie, consultar los datos
#' conjuntamente con los de otra estación cuya serie sí esté completa lo que
#' garantiza que la respuesta del servicio contiene todos los nodos y deja como
#' NA los datos ausentes.
#'
#' @param idsesion Character. Identificador de sesión válido obtenido previamente.
#' @param cestaciones Character vector. Identificadores de las estaciones.
#' @param cvariables Character vector. Identificadores de las variables.
#' @param fecha_ini Character. Fecha inicial en formato DD/MM/AAAA.
#' @param fecha_fin Character. Fecha final en formato DD/MM/AAAA.
#'
#' @return data.frame con columnas: cestacion, cvariable, fecha, valor.
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' cestaciones <- c('EARM22','SIVA40')
#' cvariables <- c('TD1','PD23')
#' fecha_ini <- '15/10/2025'
#' fecha_fin <- '26/10/2025'
#' result_diarios_intervalo <- getwsDatosDiarios2(idsesion,cestaciones,cvariables,fecha_ini,fecha_fin)
#' head(result_diarios_intervalo)
#' }
#'
#' @importFrom RCurl postForm
#' @importFrom XML xmlParse getNodeSet xmlChildren xmlValue
#' @export
#'
getwsDatosDiarios2 <- function(idsesion,cestaciones,cvariables,fecha_ini,fecha_fin) {

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
  fecha_ini_df <- getwsFechas(as.character(fecha_ini),idsesion)
  fecha_fin_df <- getwsFechas(as.character(fecha_fin),idsesion)
  pkfec_ini <- fecha_ini_df$PKFEC
  pkfec_fin <- fecha_fin_df$PKFEC

  # Generar todas las fechas entre ambas (inclusive)
  fechas <- seq(from = as.Date(fecha_ini, format = "%d/%m/%Y"), to = as.Date(fecha_fin, format = "%d/%m/%Y"), by = "day")

  # Se obtienen pkfecs de fechas
  fechas_df <- data.frame()
  for (k in 1:length(fechas)){
    fecha <- format(fechas[k], format = "%d/%m/%Y")
    fechas_df_row <- getwsFechas(fecha,idsesion)
    fechas_df <- rbind(fechas_df,fechas_df_row)
  }

  # Se consultan los datos brutos con los parametros obtenidos
  datos_df <- getwsDatosDiarios2Raw(idsesion,pkests,pkvars,pkfec_ini,pkfec_fin)

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

  message(paste0("Obtenido dataframe de datos diarios con ",nrow(df)," registros."))

  return(df)
}

