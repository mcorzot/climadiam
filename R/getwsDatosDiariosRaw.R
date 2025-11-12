#' @title Obtener un dato diario a partir de los identificadores internos de la
#' estación, variable y fecha y devolverlos como data.frame
#'
#' @description Esta función realiza una llamada SOAP al servicio de clima de la
#' Junta de Andalucía para obtener un dato diario a partir de los identificadores
#' internos de la estación, la variable y la fecha, y lo devuelve en un data.frame.
#'
#' @param idsesion Character. Identificador de sesión válido obtenido previamente.
#' @param pkest Character. Identificador interno de la estación.
#' @param pkvar Character. Identificador interno de la variable.
#' @param pkfec Character. Identificador interno de la fecha.
#'
#' @return data.frame con columnas: pkest, pkvar, pkfec, valor.
#'
#' @examples
#' \dontrun{
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' pkest <- '183' # EARM22
#' pkvar <- '2' # TD1
#' pkfec <- '100741' # 26/10/2025
#' result_diaria <- getwsDatosDiariosRaw(idsesion,pkest,pkvar,pkfec)
#' head(result_diaria)
#' }
#'
#' @importFrom RCurl postForm
#' @importFrom XML xmlParse getNodeSet xmlChildren xmlValue xmlSize
#' @export
#'
getwsDatosDiariosRaw <- function(idsesion,pkest,pkvar,pkfec) {

  # URL del servicio SOAP
  url <- "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima"

  # Construir body SOAP
  soapBody <- paste0(
    '<?xml version="1.0" encoding="utf-8"?>',
    '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ',
    'xmlns:def="http://DefaultNamespace">',
    '<soapenv:Header/>',
    '<soapenv:Body>',
    '<def:getDatosDiarios>',
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
    '</def:getDatosDiarios>',
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
        verbose = FALSE,
        followlocation = TRUE
      )
    ),
    name = "getDatosDiarios (SOAP)"
  )

  # Parsear XML
  xml_doc <- xmlParse(response)
  ns <- c(soapenv = "http://schemas.xmlsoap.org/soap/envelope/")
  body_node <- getNodeSet(xml_doc, "//soapenv:Body", namespaces = ns)[[1]]

  # Obtener el nodo principal del array de retorno
  main_node <- getNodeSet(body_node, ".//getDatosDiariosReturn")[[1]]

  # Inicializar lista de resultados
  result_list <- list()

  # Funcion recursiva para recorrer arrays anidados
  parse_node <- function(node, estacion_idx = 1, variable_idx = 1, fecha_idx = 1) {
    children <- xmlChildren(node)

    # Si llegamos al nivel con valores para cada fecha
    if (length(children) > 0 && all(sapply(children, xmlSize) == 0)) {
      for (i in seq_along(children)) {
        valor <- xmlValue(children[[i]])
        result_list[[length(result_list) + 1]] <<- data.frame(
          pkest = pkest[estacion_idx],
          pkvar = pkvar[variable_idx],
          pkfec = pkfec[i],
          valor = as.numeric(valor),
          stringsAsFactors = FALSE
        )
      }
    } else {
      # Recorrer siguiente nivel
      for (i in seq_along(children)) {
        parse_node(
          children[[i]],
          estacion_idx = estacion_idx,
          variable_idx = variable_idx,
          fecha_idx = fecha_idx
        )
      }
    }
  }

  parse_node(main_node)

  # Combinar resultados
  df <- do.call(rbind, result_list)

  # Aviso en caso de que no se encuentren resultados
  if(nrow(df) == 0){
    message("No se han encontrado datos diarios")
  } else {
    message(paste0("Obtenido dataframe de datos diarios con ",nrow(df)," registros."))
  }

  return(df)
}

