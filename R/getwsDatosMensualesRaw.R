#' @title Obtener datos mensuales a partir de los identificadores internos de
#' estaciones, variables y fechas y devolverlos como data.frame
#'
#' @description Esta función realiza una llamada SOAP al servicio de clima de la
#' Junta de Andalucía para obtener datos mensuales de varias estaciones, variables
#' y fechas, a partir de sus identificadores internos, y devolverlos como data.frame.
#'
#' @param idsesion Character. Identificador de sesión válido obtenido previamente.
#' @param pkests Character vector. Vector con los identificadores internos de las estaciones.
#' @param pkvars Character vector. Vector con los identificadores internos de las variables.
#' @param pkfecs Character vector. Vector con las identificadores internos de las fechas.
#'
#' @return data.frame con columnas: pkest, pkvar, pkfec, valor.
#'
#' @examples
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' pkests <- c('183') # EARM22
#' pkvars <- c('22') # TM1
#' pkfecs <- c('98981') # 31/12/2020
#' datos_mensuales <- getwsDatosMensualesRaw(idsesion,pkests,pkvars,pkfecs)
#' head(datos_mensuales)
#'
#' @importFrom RCurl postForm
#' @importFrom XML xmlParse getNodeSet xmlChildren xmlValue xmlSize
#' @export
#'
getwsDatosMensualesRaw <- function(idsesion,pkests,pkvars,pkfecs) {

  # URL del servicio SOAP
  url <- "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima"

  # Construir el cuerpo SOAP
  soapBody <- paste0(
    '<?xml version="1.0" encoding="utf-8"?>',
    '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ',
    'xmlns:def="http://DefaultNamespace">',
    '<soapenv:Header/>',
    '<soapenv:Body>',
    '<def:getDatosMensuales>',
    '<def:idsesion>', idsesion, '</def:idsesion>',
    '<def:estaciones>',
    paste0('<def:string>', pkests, '</def:string>', collapse = ""),
    '</def:estaciones>',
    '<def:variables>',
    paste0('<def:string>', pkvars, '</def:string>', collapse = ""),
    '</def:variables>',
    '<def:fechas>',
    paste0('<def:string>', pkfecs, '</def:string>', collapse = ""),
    '</def:fechas>',
    '</def:getDatosMensuales>',
    '</soapenv:Body>',
    '</soapenv:Envelope>'
  )

  # Headers SOAP
  httpHeader <- c(
    'Content-Type' = 'text/xml; charset=utf-8',
    'SOAPAction' = '""'
  )

  # Llamada SOAP
  response <- postForm(
    uri = url,
    .opts = list(
      postfields = soapBody,
      httpheader = httpHeader,
      verbose = TRUE,
      followlocation = TRUE
    )
  )

  # Parsear XML
  xml_doc <- xmlParse(response)
  ns <- c(soapenv = "http://schemas.xmlsoap.org/soap/envelope/")
  body_node <- getNodeSet(xml_doc, "//soapenv:Body", namespaces = ns)[[1]]

  # Obtener el nodo principal del array de retorno
  main_node <- getNodeSet(body_node, ".//getDatosMensualesReturn")[[1]]

  result_list <- list()

  # Nivel 1: estaciones
  estaciones_nodes <- xmlChildren(main_node)

  for (est_idx in seq_along(estaciones_nodes)) {
    est_node <- estaciones_nodes[[est_idx]]
    var_nodes <- xmlChildren(est_node)

    # Nivel 2: variables
    for (var_idx in seq_along(var_nodes)) {
      var_node <- var_nodes[[var_idx]]
      fecha_nodes <- xmlChildren(var_node)

      # Nivel 3: fechas
      for (fecha_idx in seq_along(fecha_nodes)) {
        fecha_node <- fecha_nodes[[fecha_idx]]
        valor_nodes <- xmlChildren(fecha_node)

        # Nivel 4: valores diarios (un valor por fecha)
        for (valor_node in valor_nodes) {
          valor <- as.numeric(xmlValue(valor_node))
          result_list[[length(result_list) + 1]] <- data.frame(
            pkest = pkests[est_idx],
            pkvar = pkvars[var_idx],
            pkfec = pkfecs[fecha_idx],
            valor = valor,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }

  # Combinar todos los resultados
  df <- do.call(rbind, result_list)
  return(df)
}

