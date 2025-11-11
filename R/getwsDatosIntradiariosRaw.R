#' @title Obtener datos intradiarios a partir de los identificadores internos de
#' la estación, la variable y la fecha y devolverlos como data.frame
#'
#' @description
#' Esta función realiza una llamada SOAP al servicio de clima de la Junta de
#' Andalucía para obtener datos intradiarios de una estación, variable y
#' fecha a partir de sus identificadores internos, y devuelve un data.frame.
#'
#' @param idsesion Character. Identificador de sesión válido obtenido previamente.
#' @param pkest Character. Identificador interno de la estación a consultar.
#' @param pkvar Character. Identificador interno de la variable a consultar.
#' @param pkfec Character. Identificador interno de la fecha a consultar.
#'
#' @return data.frame con columnas: id de estación, id de variable, id de fecha y valor.
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' pkest <- '183' # EARM22
#' pkvar <- '1' # TI1
#' pkfec <- '100741' # 26/10/2025
#' datos_intra <- getwsDatosIntradiariosRaw(idsesion,pkest,pkvar,pkfec)
#' head(datos_intra)
#' }
#'
#' @importFrom RCurl postForm
#' @importFrom XML xmlParse getNodeSet xmlChildren xmlValue
#' @export
#'
getwsDatosIntradiariosRaw <- function(idsesion,pkest,pkvar,pkfec) {

  # URL del servicio SOAP
  url <- "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima"

  # Construir body SOAP
  soapBody <- paste0(
    '<?xml version="1.0" encoding="utf-8"?>',
    '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ',
    'xmlns:def="http://DefaultNamespace">',
    '<soapenv:Header/>',
    '<soapenv:Body>',
    '<def:getDatosIntradiarios>',
    '<def:idsesion>', idsesion, '</def:idsesion>',
    '<def:estaciones>',
    paste0('<def:string>', pkest, '</def:string>', collapse=""),
    '</def:estaciones>',
    '<def:variables>',
    paste0('<def:string>', pkvar, '</def:string>', collapse=""),
    '</def:variables>',
    '<def:fechas>',
    paste0('<def:string>', pkfec, '</def:string>', collapse=""),
    '</def:fechas>',
    '</def:getDatosIntradiarios>',
    '</soapenv:Body>',
    '</soapenv:Envelope>'
  )

  # Headers SOAP
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

  # Extraer todos los elementos getDatosIntradiariosReturn anidados
  data_nodes <- getNodeSet(body_node, ".//getDatosIntradiariosReturn")

  # Inicializar lista de resultados
  result_list <- list()

  # Iterar sobre cada par minuto-valor
  for (node in data_nodes) {
    children <- xmlChildren(node)
    if (length(children) == 2) {
      minuto <- xmlValue(children[[1]])
      valor <- as.numeric(xmlValue(children[[2]]))
      # Agregar combinaciones de estacion, variable, fecha
      # Asumimos que la respuesta sigue el mismo orden que los arrays enviados
      result_list[[length(result_list) + 1]] <- data.frame(
        pkest = pkest[1],  # Si envías múltiples estaciones, se puede ampliar
        pkvar = pkvar[1],   # Similar para múltiples variables
        pkfec = pkfec[1],          # Similar para múltiples fechas
        minuto = as.numeric(minuto),
        valor = valor,
        stringsAsFactors = FALSE
      )
    }
  }

  # Combinar en un unico data.frame
  df <- do.call(rbind, result_list)
  return(df)
}
