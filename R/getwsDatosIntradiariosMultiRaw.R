#' @title Obtener datos intradiarios a partir de los identificadores internos de
#' la estación, la variable y la fecha y devolverlos como data.frame (multiconsulta)
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de
#' Andalucía para obtener datos intradiarios de una estación, variable y
#' fecha a partir de sus identificadores internos, y devuelve un data.frame.
#' Permite combinar varias variables y fechas para una estación única.
#'
#' @param idsesion Character. Identificador de sesión válido obtenido previamente.
#' @param pkests Character vector. Vector con los identificadores internos de las de las estaciones a consultar.
#' @param pkvars Character vector. Vector con los identificadores internos de las variables a consultar.
#' @param pkfecs Character vector. Vector con los identificadores internos de las fechas a consultar.
#'
#' @return data.frame con columnas: pkest, pkvar, pkfec, minuto, valor.
#'
#' @examples
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' pkests <- c('183') # EARM22
#' pkvars <- c('1','63') # TI1 y PI1
#' pkfecs <- c('100741','100742') # 26/10/2025 y 27/10/2025
#' datos_intra_raw <- getwsDatosIntradiariosMultiRaw(idsesion,pkests,pkvars,pkfecs)
#' head(datos_intra_raw)
#'
#' @importFrom RCurl postForm
#' @importFrom XML xmlParse getNodeSet xmlChildren xmlValue
#' @export
#'
getwsDatosIntradiariosMultiRaw <- function(idsesion,pkests,pkvars,pkfecs) {

  # URL del servicio SOAP
  url <- "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima"

  # Construir cuerpo SOAP
  soapBody <- paste0(
    '<?xml version="1.0" encoding="utf-8"?>',
    '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ',
    'xmlns:def="http://DefaultNamespace">',
    '<soapenv:Header/>',
    '<soapenv:Body>',
    '<def:getDatosIntradiarios>',
    '<def:idsesion>', idsesion, '</def:idsesion>',
    '<def:estaciones>',
    paste0('<def:string>', pkests, '</def:string>', collapse=""),
    '</def:estaciones>',
    '<def:variables>',
    paste0('<def:string>', pkvars, '</def:string>', collapse=""),
    '</def:variables>',
    '<def:fechas>',
    paste0('<def:string>', pkfecs, '</def:string>', collapse=""),
    '</def:fechas>',
    '</def:getDatosIntradiarios>',
    '</soapenv:Body>',
    '</soapenv:Envelope>'
  )

  httpHeader <- c(
    'Content-Type' = 'text/xml; charset=utf-8',
    'SOAPAction' = '""'
  )

  # Llamada SOAP
  response <- call_with_retry(
    expr = RCurl::postForm(
      uri = url,
      .opts = list(
        postfields = soapBody,
        httpheader = httpHeader,
        verbose = TRUE,
        followlocation = TRUE
      )
    ),
    name = "getDatosIntradiarios (SOAP)"
  )

  # Parsear XML
  xml_doc <- xmlParse(response)
  ns <- c(soapenv = "http://schemas.xmlsoap.org/soap/envelope/")
  body_node <- getNodeSet(xml_doc, "//soapenv:Body", namespaces = ns)[[1]]

  # Obtener el nodo principal de retorno
  main_node <- getNodeSet(body_node, ".//getDatosIntradiariosReturn")[[1]]

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
          hijos <- xmlChildren(valor_node)
          if (length(hijos) == 2) {
            minuto <- as.numeric(xmlValue(hijos[[1]]))
            valor <- as.numeric(xmlValue(hijos[[2]]))
            result_list[[length(result_list) + 1]] <- data.frame(
              pkest = pkests[est_idx],
              pkvar = pkvars[var_idx],
              pkfec = pkfecs[fecha_idx],
              minuto = minuto,
              valor = valor,
              stringsAsFactors = FALSE
          )
        }}
      }
    }
    }
  df <- do.call(rbind, result_list)
  return(df)
}
