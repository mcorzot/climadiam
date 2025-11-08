###############################################################################
# 📘 PRUEBA PRÁCTICA: 
# SOBRE UNA SELECCIÓN DE VARIABLES, OBTENER LOS DATOS INTRADIARIOS DE AYER 
# Y LOS DATOS DIARIOS DE ANTES DE AYER
#
# Paquete: climadiam
# Autor: mcorzot
# Fecha: Sys.Date()
###############################################################################

# -----------------------------------------------------------------------------
# 🔧 CONFIGURACIÓN INICIAL
# -----------------------------------------------------------------------------

# Cargar el paquete
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
existencias$FECINICIODATOS <- as.Date(existencias$FECINICIODATOS, format = "%Y-%m-%d %H:%M:%S.0")
existencias$FECFINDATOS    <- as.Date(existencias$FECFINDATOS,    format = "%Y-%m-%d %H:%M:%S.0")

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
head(sel_estaciones)

# Características de las variables seleccionadas
cvariables <- c(
  "TI1", "HI1", "PI1", "VI2",
  "TD1", "TD2", "TD4",
  "PD23", "HD1", "HD2", "HD4", "VD51"
)

variables <- data.frame()

for (cvariable in cvariables) {
  sel_variables <- getwsVariables(cvariable, idsesion)
  variables <- rbind(variables, sel_variables)
}

head(variables)

# -----------------------------------------------------------------------------
# ⏱️ CONSULTA DE DATOS INTRADIARIOS (DÍA MENOS 1)
# -----------------------------------------------------------------------------

# Variables intradiarias
cvariables_int <- c("TI1", "HI1", "PI1", "VI2")
fechas_int     <- date1_char
datos_intradiarios1 <- data.frame()

# Fragmentar la consulta por red para evitar saturar el servicio
for (red in unique(sel_estaciones$REDPKRED)) {
  sel_estaciones_red <- subset(sel_estaciones, REDPKRED == red)
  sel_cestaciones    <- sel_estaciones_red$CESTACION
  
  sel_datos_intradiarios1 <- getwsDatosIntradiariosMulti(
    idsesion,
    sel_cestaciones,
    cvariables_int,
    fechas_int
  )
  
  datos_intradiarios1 <- rbind(datos_intradiarios1, sel_datos_intradiarios1)
}

head(datos_intradiarios1)

# -----------------------------------------------------------------------------
# 📊 CONSULTA DE DATOS DIARIOS (DÍA MENOS 2)
# -----------------------------------------------------------------------------

cestaciones   <- sel_existencias$CESTACION
cvariables_dia <- c("TD1", "TD2", "TD4", "PD23", "HD1", "HD2", "HD4", "VD51")
fechas_dia     <- date2_char

datos_diarios2 <- getwsDatosDiariosMulti(
  idsesion,
  cestaciones,
  cvariables_dia,
  fechas_dia
)

head(datos_diarios2)

# -----------------------------------------------------------------------------
# ⚠️ NOTA FINAL
# -----------------------------------------------------------------------------
# No se recomienda la consulta de datos mensuales o anuales, ya que los 
# criterios de agregación y validación hacen que estos sean muy escasos 
# en la base de datos.
# -----------------------------------------------------------------------------

# 🎯 FIN DEL SCRIPT
###############################################################################
