###############################################################################
# 📘 PRUEBA PRÁCTICA:
# SOBRE UNA SELECCIÓN DE VARIABLES, OBTENER LOS DATOS INTRADIARIOS DE AYER
# Y LOS DATOS DIARIOS DE ANTES DE AYER
#
# Paquete: climadiam
# Autor: Mariano Corzo Toscano
# Fecha: Sys.Date()
###############################################################################

# -----------------------------------------------------------------------------
# 🔧 CONFIGURACIÓN INICIAL
# -----------------------------------------------------------------------------

# Si es necesario, instalar el paquete 'remotes'
#install.packages("remotes")
#library(remotes)

# Si es necesario, usando remotes, instalar el paquete 'climadiam' desde GitHub
#remotes::install_github("mcorzot/climadiam")

# Cargar el paquete climadiam
library(climadiam)

# Usuario y contraseña de acceso libre
user     <- "usuario"
password <- "usuario"

# -----------------------------------------------------------------------------
# 🔐 SESIÓN DE TRABAJO
# -----------------------------------------------------------------------------

# Obtener el ID de sesión
idsesion <- getwsIDSesion(user, password)
idsesion

# -----------------------------------------------------------------------------
# 📅 DEFINICIÓN DE FECHAS
# -----------------------------------------------------------------------------

# Día menos 1 (ayer)
date1      <- Sys.Date() - 1
date1_char <- format(date1, format = "%d/%m/%Y")
sel_fecha1 <- getwsFechas(date1_char, idsesion)
sel_fecha1$FECHA

# Día menos 2 (antes de ayer)
date2      <- Sys.Date() - 2
date2_char <- format(date2, format = "%d/%m/%Y")
sel_fecha2 <- getwsFechas(date2_char, idsesion)
sel_fecha2$FECHA

# -----------------------------------------------------------------------------
# 🗂️ EXISTENCIAS DE DATOS Y SELECCIÓN DE ESTACIONES
# -----------------------------------------------------------------------------

# Obtener existencias genéricas de datos
existencias <- getwsDatosExisteEstacionesList(idsesion)

# Convertir fechas de inicio y fin a formato Date
existencias$FECINICIODATOS <- as.Date(existencias$FECINICIODATOS,
                                      format = "%Y-%m-%d %H:%M:%S.0")
existencias$FECFINDATOS    <- as.Date(existencias$FECFINDATOS,
                                      format = "%Y-%m-%d %H:%M:%S.0")

# Filtrar estaciones con datos válidos para el día menos 2
sel_existencias <- existencias[
  !is.na(existencias$FECINICIODATOS) &
  existencias$FECINICIODATOS < date2 &
  existencias$FECFINDATOS > date2,
]
head(sel_existencias)

# -----------------------------------------------------------------------------
# 🏗️ INFORMACIÓN DE ESTACIONES Y VARIABLES
# -----------------------------------------------------------------------------

# Características básicas de las estaciones seleccionadas
estaciones     <- getwsEstacionesList(idsesion)
sel_estaciones <- estaciones[estaciones$CESTACION %in% sel_existencias$CESTACION, ]
# Se descartan las estaciones PART003 y CAHA01 dado que el servicio falla al
# devolver datos con intervalos de medida inferiores a 10 minutos
sel_estaciones <- sel_estaciones[sel_estaciones$CESTACION != "PART003", ]
sel_estaciones <- sel_estaciones[sel_estaciones$CESTACION != "CAHA01", ]
head(sel_estaciones)

# Características de las variables seleccionadas
cvariables <- c("TI1", "HI1", "PI1", "VI2","TD1", "TD2", "TD4","PD23", "HD1", "HD2", "HD4", "VD51")
variables <- data.frame()
for (cvariable in cvariables) {
  sel_variables <- getwsVariables(cvariable, idsesion, retries = 5, wait = 3)
  variables <- rbind(variables, sel_variables)
}
head(variables)

# -----------------------------------------------------------------------------
# ⏱️ CONSULTA DE DATOS INTRADIARIOS (DÍA MENOS 1)
# -----------------------------------------------------------------------------

# Variables intradiarias
cvariables_int <- c("TI1", "HI1", "PI1", "VI2")
fechas_int     <- date1_char

# Se crea dataframe en blanco en el que incorporar los datos
datos_intradiarios1 <- data.frame()

# Consulta con la función "getwsDatosIntradiariosMulti" y los identificadores
# Se fragmenta la consulta por red para evitar saturar el servicio
for (red in unique(sel_estaciones$REDPKRED)) {
  sel_estaciones_red <- subset(sel_estaciones, REDPKRED == red)
  sel_cestaciones    <- sel_estaciones_red$CESTACION
  sel_datos_intradiarios1 <- getwsDatosIntradiariosMulti(idsesion,sel_cestaciones,
                                                         cvariables_int,fechas_int)
  datos_intradiarios1 <- rbind(datos_intradiarios1, sel_datos_intradiarios1)
}

head(datos_intradiarios1)

# -----------------------------------------------------------------------------
# ⏱️ ALTERNATIVA PARA LA CONSULTA DE DATOS INTRADIARIOS (DÍA MENOS 1)
#     EN CASO DE INESTABILIDAD DE LOS SERVICIOS
# -----------------------------------------------------------------------------

# Se usa la función "getwsDatosIntradiariosMultiRaw" que requiere los
# identificadores internos de los parámetros de la consulta

# Obtener identificadores internos de las variables intradiarias
cvariables_int <- c("TI1", "HI1", "PI1", "VI2")
variables <- getwsVariablesList(idsesion) # lista de variables
sel_variables <- variables[variables$CVARIABLE %in% cvariables_int,]
pkvars_int <- sel_variables$PKVAR

# Obtener identificadores internos de las fechas
date1      <- Sys.Date() - 1
date1_char <- format(date1, format = "%d/%m/%Y")
fechas <- getwsFechasList(idsesion)
fecha <- fechas[fechas$FECHA == date1_char,]
pkfec_int <- sel_fecha1$PKFEC
pkfec_int_prev <- as.numeric(sel_fecha1$PKFEC) - 1 # pkfec del día previo

# Obtener lista de estaciones para luego iterar por ellas
estaciones <- getwsEstacionesList(idsesion) # lista de estaciones
sel_estaciones <- estaciones[estaciones$CESTACION %in% sel_existencias$CESTACION, ]
# Se descartan las estaciones PART003 y CAHA01 dado que el servicio falla al devolver datos con
# intervalos de medida inferiores a 10 minutos
sel_estaciones <- sel_estaciones[sel_estaciones$CESTACION != "PART003", ]
sel_estaciones <- sel_estaciones[sel_estaciones$CESTACION != "CAHA01", ]

# Se crea dataframe en blanco para almacenar los datos
datos_intradiarios1_alt <- data.frame()

# Consulta con la función "getwsDatosIntradiariosMultiRaw" y los identificadores internos
# Se obtienen datos del día objetivo y el día previo ya que en CLIMA el dato del día 1 a las 00:00
# se almacena como el dato de las 24:00 del día anterior
# Fragmentar la consulta por red para evitar saturar el servicio
for (red in unique(sel_estaciones$REDPKRED)) {
  sel_estaciones_red <- subset(sel_estaciones, REDPKRED == red)
  sel_pkests    <- sel_estaciones_red$PKEST
  # Consulta día previo
  sel_datos_intradiarios1_alt_prev <- getwsDatosIntradiariosMultiRaw(idsesion,sel_pkests,pkvars_int,pkfec_int_prev)
  # Consulta día objetivo
  sel_datos_intradiarios1_alt <- getwsDatosIntradiariosMultiRaw(idsesion,sel_pkests,pkvars_int,pkfec_int)
  # Se anexan los resultados
  datos_intradiarios1_alt <- rbind(datos_intradiarios1_alt,sel_datos_intradiarios1_alt_prev,sel_datos_intradiarios1_alt)
}
head(datos_intradiarios1_alt)

# Se borran los registros en blanco derivados de la estructura de la respuesta del servicio
datos_intradiarios1_alt <- datos_intradiarios1_alt[complete.cases(datos_intradiarios1_alt), ]

# Obtenidos los datos en bruto, se relacionan con las tablas maestras
# Se relaciona con las horas-minutos
datos_intradiarios1_alt <- merge(x = datos_intradiarios1_alt, y = minutos_horas, by.x='minuto', by.y='MINUTOS')

# Se convierte el dato de las 24:00 en el de las 00:00 del dia siguiente
datos_intradiarios1_alt$HORA[datos_intradiarios1_alt$HORA=="24:00"] <- "00:00"
datos_intradiarios1_alt$pkfec <- ifelse(datos_intradiarios1_alt$HORA %in% "00:00", as.character(as.integer(datos_intradiarios1_alt$pkfec)+1), datos_intradiarios1_alt$pkfec)

# Se filtra teniendo en cuenta la fecha indicada en la consulta
datos_intradiarios1_alt <- datos_intradiarios1_alt[datos_intradiarios1_alt$pkfec %in% pkfec_int,]

# Se borran duplicados si los hubiera
datos_intradiarios1_alt <- datos_intradiarios1_alt[!duplicated(datos_intradiarios1_alt), ]

# Se ordena el dataframe por hora
datos_intradiarios1_alt <- datos_intradiarios1_alt[order(datos_intradiarios1_alt$pkest,datos_intradiarios1_alt$pkvar,datos_intradiarios1_alt$pkfec,datos_intradiarios1_alt$HORA),]

# Se asocian los identificadores internos con los valores aportados en la funcion
datos_intradiarios1_alt <- merge(x = datos_intradiarios1_alt, y = estaciones, by.x = 'pkest', by.y = 'PKEST')
datos_intradiarios1_alt <- merge(x = datos_intradiarios1_alt, y = variables, by.x = 'pkvar', by.y = 'PKVAR')
datos_intradiarios1_alt <- merge(x = datos_intradiarios1_alt, y = fechas, by.x = 'pkfec', by.y = 'PKFEC')

# Se compone el dataframe
datos_intradiarios1_alt <- data.frame(datos_intradiarios1_alt$CESTACION,datos_intradiarios1_alt$CVARIABLE,datos_intradiarios1_alt$FECHA,datos_intradiarios1_alt$HORA,datos_intradiarios1_alt$valor)
colnames(datos_intradiarios1_alt) <- c('cestacion','cvariable','fecha','hora','valor')
head(datos_intradiarios1_alt)

# -----------------------------------------------------------------------------
# 📊 CONSULTA DE DATOS DIARIOS (DÍA MENOS 2)
# -----------------------------------------------------------------------------

# Se usa la función "getwsDatosDiariosMulti" que requiere los
# identificadores de los parámetros de la consulta

cestaciones   <- sel_existencias$CESTACION
cvariables_dia <- c("TD1", "TD2", "TD4", "PD23", "HD1", "HD2", "HD4", "VD51")
fechas_dia <- date2_char
# Consulta con la función "getwsDatosDiariosMulti" y los identificadores
datos_diarios2 <- getwsDatosDiariosMulti(idsesion,cestaciones,cvariables_dia,fechas_dia)
head(datos_diarios2)

# -----------------------------------------------------------------------------
# ⏱️ ALTERNATIVA PARA LA CONSULTA DE DATOS DIARIOS (DÍA MENOS 2)
#     EN CASO DE INESTABILIDAD DE LOS SERVICIOS
# -----------------------------------------------------------------------------

# Se usa la función "getwsDatosDiariosMultiRaw" que requiere los
# identificadores internos de los parámetros de la consulta

# Obtener identificadores internos de las variables diarias
cvariables_dia <- c("TD1", "TD2", "TD4", "PD23", "HD1", "HD2", "HD4", "VD51")
variables <- getwsVariablesList(idsesion) # lista de variables
sel_variables_dia <- variables[variables$CVARIABLE %in% cvariables_dia,]
pkvars_dia <- sel_variables_dia$PKVAR

# Obtener identificadores internos de las fechas
fechas_dia <- date2_char
date2_char <- format(date2, format = "%d/%m/%Y")
fechas <- getwsFechasList(idsesion)
fecha <- fechas[fechas$FECHA == date2_char,]
pkfec_dia <- fecha$PKFEC

# Obtener lista de estaciones para luego iterar por ellas
estaciones <- getwsEstacionesList(idsesion) # lista de estaciones
sel_estaciones <- estaciones[estaciones$CESTACION %in% sel_existencias$CESTACION, ]
# Se descartan las estaciones PART003 y CAHA01 dado que el servicio falla al devolver datos con
# intervalos de medida inferiores a 10 minutos
sel_estaciones <- sel_estaciones[sel_estaciones$CESTACION != "PART003", ]
sel_estaciones <- sel_estaciones[sel_estaciones$CESTACION != "CAHA01", ]

# Se crea dataframe en blanco para almacenar los datos
datos_diarios2_alt <- data.frame()

# Consulta con la función "getwsDatosDiariosMultiRaw" y los identificadores internos
# Se obtienen datos del día objetivo y el día previo ya que en CLIMA el dato del día 1 a las 00:00
# se almacena como el dato de las 24:00 del día anterior
# Fragmentar la consulta por red para evitar saturar el servicio
for (red in unique(sel_estaciones$REDPKRED)) {
  sel_estaciones_red <- subset(sel_estaciones, REDPKRED == red)
  sel_pkests    <- sel_estaciones_red$PKEST
  # Consulta día objetivo
  sel_datos_diarios2_alt <- getwsDatosDiariosMultiRaw(idsesion,sel_pkests,pkvars_dia,pkfec_dia)
  # Se anexan los resultados
  datos_diarios2_alt <- rbind(datos_diarios2_alt,sel_datos_diarios2_alt)
}
head(datos_diarios2_alt)

# Se ordena el dataframe por estación, variable y fecha
datos_diarios2_alt <- datos_diarios2_alt[order(datos_diarios2_alt$pkest,datos_diarios2_alt$pkvar,datos_diarios2_alt$pkfec),]

# Se asocian los identificadores internos con los valores aportados en la funcion
datos_diarios2_alt <- merge(x = datos_diarios2_alt, y = estaciones, by.x = 'pkest', by.y = 'PKEST')
datos_diarios2_alt <- merge(x = datos_diarios2_alt, y = variables, by.x = 'pkvar', by.y = 'PKVAR')
datos_diarios2_alt <- merge(x = datos_diarios2_alt, y = fechas, by.x = 'pkfec', by.y = 'PKFEC')

# Se compone el dataframe
datos_diarios2_alt <- data.frame(datos_diarios2_alt$CESTACION,datos_diarios2_alt$CVARIABLE,datos_diarios2_alt$FECHA,datos_diarios2_alt$valor)
colnames(datos_diarios2_alt) <- c('cestacion','cvariable','fecha','valor')
head(datos_diarios2_alt)

# -----------------------------------------------------------------------------
# ⚠️ NOTA FINAL
# -----------------------------------------------------------------------------
# No se recomienda la consulta de datos mensuales o anuales, ya que los
# criterios de agregación y validación hacen que estos sean muy escasos
# en la base de datos.
# -----------------------------------------------------------------------------

# 🎯 FIN DEL SCRIPT
###############################################################################


