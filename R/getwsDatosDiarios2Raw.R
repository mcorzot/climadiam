#' @title Obtener datos diarios de un periodo a partir de los identificadores
#' internos de estaciones, variables y fechas, y devolverlos como data.frame
#'
#' @description
#' Llama al método \code{getDatosDiarios2} del servicio CLIMA de la Junta de
#' Andalucía para obtener datos diarios de varias estaciones y variables entre
#' dos PKFEC (identificadores numéricos de fecha). Los PKFEC, se generan
#' secuencialmente entre \code{pkfec_ini} y \code{pkfec_fin} por lo que, en
#' caso de lagunas, pueden no ser correctos.
#'
#' Se recomienda, en caso de que haya lagunas en la serie, consultar los datos
#' conjuntamente con los de otra estación cuya serie sí esté completa lo que
#' garantiza que la respuesta del servicio contiene todos los nodos y deja como
#' NA los datos ausentes.
#'
#' @param idsesion Character. Identificador de sesión válido obtenido previamente.
#' @param pkests Character vector. Identificadores internos de las estaciones.
#' @param pkvars Character vector. Identificadores internos de las variables.
#' @param pkfec_ini Character. Identificador interno de la fecha inicial.
#' @param pkfec_fin Character. Identificador interno de la fecha final.
#'
#' @return data.frame con columnas: pkest, pkvar, pkfec, valor.
#'
#' @examples
#' user <- 'usuario'
#' password <- 'usuario'
#' idsesion <- getwsIDSesion(user,password)
#' pkests <- c("183","143")
#' pkvars <- c('2','711') # TD1 y PD23
#' pkfec_ini <- '100730' # 15/10/2025
#' pkfec_fin <- '100741' # 26/10/2025
#' result_diarios <- getwsDatosDiarios2Raw(idsesion,pkests,pkvars,pkfec_ini,pkfec_fin)
#' head(result_diarios)
#'
#' @importFrom RCurl postForm
#' @importFrom XML xmlParse getNodeSet xmlChildren xmlValue
#' @export
#'
getwsDatosDiarios2Raw <- function(idsesion,pkests,pkvars,pkfec_ini,pkfec_fin) {

  # Asegurar que los PKFEC sean numericos
  pkfec_ini <- as.integer(pkfec_ini)
  pkfec_fin <- as.integer(pkfec_fin)

  # URL del servicio SOAP
  url <- "http://www.juntadeandalucia.es/medioambiente/servtc5/climaws/services/ServicioClima"

  # Cuerpo SOAP
  soapBody <- paste0(
    '<?xml version="1.0" encoding="utf-8"?>',
    '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" ',
    'xmlns:def="http://DefaultNamespace">',
    '<soapenv:Header/>',
    '<soapenv:Body>',
    '<def:getDatosDiarios2>',
    '<def:PKSESION>', idsesion, '</def:PKSESION>',
    '<def:estaciones>',
    paste0('<def:string>', pkests, '</def:string>', collapse = ""),
    '</def:estaciones>',
    '<def:variables>',
    paste0('<def:string>', pkvars, '</def:string>', collapse = ""),
    '</def:variables>',
    '<def:pkfec_inicio>', pkfec_ini, '</def:pkfec_inicio>',
    '<def:pkfec_fin>', pkfec_fin, '</def:pkfec_fin>',
    '</def:getDatosDiarios2>',
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
      verbose = FALSE,
      followlocation = TRUE
    )
  )

  # Parsear XML
  xml_doc <- xmlParse(response)
  ns <- c(soapenv = "http://schemas.xmlsoap.org/soap/envelope/")
  body_node <- getNodeSet(xml_doc, "//soapenv:Body", namespaces = ns)[[1]]
  main_node <- getNodeSet(body_node, ".//getDatosDiarios2Return")[[1]]

  result_list <- list()

  # Secuencia de PKFEC (entera)
  pkfec_seq <- seq(pkfec_ini, pkfec_fin, by = 1)

  # --- Nivel 1: estaciones ---
  estaciones_nodes <- xmlChildren(main_node)

  for (est_idx in seq_along(estaciones_nodes)) {
    est_node <- estaciones_nodes[[est_idx]]
    var_nodes <- xmlChildren(est_node)

    # --- Nivel 2: variables ---
    for (var_idx in seq_along(var_nodes)) {
      var_node <- var_nodes[[var_idx]]
      valor_nodes <- xmlChildren(var_node)

      # Extraer valores
      valores <- as.numeric(sapply(valor_nodes, xmlValue))
      n_vals <- length(valores)

      # Ajustar longitud del PKFEC segun valores devueltos
      pkfec_usados <- pkfec_seq[seq_len(min(length(pkfec_seq), n_vals))]

      result_list[[length(result_list) + 1]] <- data.frame(
        pkest = pkests[est_idx],
        pkvar = pkvars[var_idx],
        pkfec = pkfec_usados,
        valor = valores[seq_len(length(pkfec_usados))],
        stringsAsFactors = FALSE
      )
    }
  }

  df <- do.call(rbind, result_list)
  if(nrow(df)< (length(pkfec_seq))){
    message('Hay menos datos diarios de los esperados. La asignaci\u00F3n de fechas a los mismos no es correcta')
    }
  return(df)
}

