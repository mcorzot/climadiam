#' Tabla de equivalencia de minutos a horas
#'
#'
#' @format Un \code{data.frame} con las siguientes columnas:
#' \describe{
#'   \item{minutos}{Número de minutos en el día, de 0 a 1440}
#'   \item{horas}{Hora en formato HH:MM}
#' }
#'
#' @source Datos preprocesados internamente por el paquete.
#'
#' @examples
#' df <- minutos_horas
#' head(minutos_horas)
"minutos_horas"
