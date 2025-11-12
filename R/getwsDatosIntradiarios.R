#' @title Obtener datos intradiarios de una estación, variable y fecha y devolverlos
#' como data.frame
#'
#' @description Esta función realiza una llamada SOAP al servicio de clima de la
#' Junta de Andalucía para obtener datos intradiarios de una estación, variable y
#' fecha, y devuelve un data.frame.
#'
#' @param idsesion Character. Identificador de sesión válido obtenido previamente.
#' @param cestacion Character. Identificador de la estación a consultar.
#' @param cvariable Character. Identificador de la variable a consultar.
#' @param fecha Character. Fecha en formato DD/MM/AAAA.
#'
#' @return data.frame con columnas: código de estación, variable, fecha, hora y valor.
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' cestacion <- 'EARM22'
#' cvariable <- 'TI1'
#' fecha <- '26/10/2025'
#' datos_intra <- getwsDatosIntradiarios(idsesion,cestacion,cvariable,fecha)
#' head(datos_intra)
#' }
#'
#' @importFrom RCurl postForm
#' @importFrom XML xmlParse getNodeSet xmlChildren xmlValue
#' @export
#'
getwsDatosIntradiarios <- function(idsesion,cestacion,cvariable,fecha) {

  # Se obtiene pkest de cestacion
  estacion <- getwsEstaciones(cestacion,idsesion)
  pkest <- estacion$PKEST

  # Se obtiene pkvar de cvariable
  variable <- getwsVariables(cvariable,idsesion)
  pkvar <- variable$PKVAR

  # Se obtiene pkfec de fecha
  fecha_df <- getwsFechas(fecha,idsesion)
  pkfec <- fecha_df$PKFEC
  pkfec_prev <- as.character(as.integer(pkfec)-1)

  # Se consultan los datos raw del día objetivo y el previo
  df_prev <- getwsDatosIntradiariosRaw(idsesion,pkest,pkvar,pkfec_prev)
  df <- getwsDatosIntradiariosRaw(idsesion,pkest,pkvar,pkfec)
  df <- rbind(df_prev,df)

  # En caso de que la consulta no obtenga registros se genera dataframe vacio
  if (nrow(df) == 0){
    df <- data.frame(
      cestacion = character(),
      cvariable = character(),
      fecha = character(),
      hora = character(),
      valor = numeric(),
      stringsAsFactors = FALSE
    )
  } else {

    # Se relaciona con las horas-minutos
    df <- merge(x = df, y = minutos_horas, by.x='minuto', by.y='MINUTOS')

    # Se convierte el dato de las 24:00 en el de las 00:00 del dia siguiente
    df$HORA[df$HORA=="24:00"] <- "00:00"
    df$pkfec <- ifelse(df$HORA %in% "00:00", as.character(as.integer(df$pkfec)+1), df$pkfec)

    # Se filtra teniendo en cuenta la fecha indicada en la consulta
    df <- df[df$pkfec == pkfec,]

    # Se ordena el dataframe por hora
    df <- df[order(df$HORA),]

    # Se compone el dataframe
    df <- data.frame(cestacion,cvariable,fecha,df$HORA,df$valor)
    colnames(df) <- c('cestacion','cvariable','fecha','hora','valor')
  }

  message(paste0("Obtenido dataframe de datos intradiarios con ",nrow(df)," registros."))

  return(df)
}
