# climadiam
Algunos ejemplos de funciones de consulta de datos del Subsistema CLIMA

# INSTALAR PAQUETE
install.packages("remotes")
remotes::install_github("mcorzot/climadiam")

# CARGA DE PAQUETE
library(climadiam)

# USUARIO Y CONTRASEÑA DE ACCESO LIBRE
user  <- 'usuario'
password <- 'usuario'

# OBTENER EL ID DE SESION
idsesion <- getwsIDSesion(user,password)

# TABLAS MAESTRAS
comarcas <- getwsComarcasList(idsesion)
areas_geograficas <- getwsAreasGeograficasList(idsesion)
areas_climaticas <- getwsAreasClimaticasList(idsesion)
escalas <- getwsEscalasList(idsesion)
gestores <- getwsGestoresList(idsesion)
redes <- getwsRedesList(idsesion)
magnitudes <- getwsMagnitudesList(idsesion)
unidades <- getwsUnidadesList(idsesion)
provincias <- getwsProvinciasList(idsesion)
municipios <- getwsMunicipiosList(idsesion)
redes <- getwsRedesList(idsesion)
estaciones <- getwsEstacionesList(idsesion)
estados_datos <- getwsEstadosDatosList(idsesion)
estaciones_detalle <- getwsEstacionesExpandidoList(idsesion)
tipos_medias <- getwsTiposMediasList(idsesion)
variables <- getwsVariablesList(idsesion)
fechas <- getwsFechasList(idsesion)
# CONSULTA DE FECHA CONCRETA
date <- format(as.Date(Sys.Date()-4,format='%Y-%m-%d'), format = '%d/%m/%Y')
fecha <- getwsFechas(as.character(date),idsesion)

# CONSULTA DE VARIABLES POR ESTACION CONCRETA (RAW)
varpkvar <- '1'
variablesestacioneslistparams_raw <- getwsVariablesEstacionesListRaw(varpkvar,idsesion)

# CONSULTA DE VARIABLES POR ESTACION CONCRETA (INT)
cvariable <- 'TI1'
variablesestacioneslistparams_int <- getwsVariablesEstacionesList(cvariable,idsesion)

# CONSULTA DE LAS EXISTENCIA DE DATOS POR ESTACION RAW
pkest <- '183'
existencias_estacion_raw <- getwsDatosExisteEstacionesListRaw(idsesion,pkest)

# CONSULTA DE LAS EXISTENCIA DE DATOS POR ESTACION INT
cestacion <- 'EARM22'
existencias_estacion_int <- getwsDatosExisteEstacionesList(idsesion,cestacion)

# CONSULTAR DATOS DIARIOS RAW (UNA ESTACION, UNA VARIABLE, UNA FECHA)
pkest <- '183' #EARM22
pkvar <- '2' # TD1
pkfec <- '100741'
result_diaria_raw <- getwsDatosDiariosRaw(idsesion, pkest, pkvar, pkfec)

# CONSULTAR DATOS DIARIOS INT (UNA ESTACION, UNA VARIABLE, UNA FECHA)
cestacion <- 'EARM22'
cvariable <- 'TD1'
fecha <- '26/10/2025'
result_diaria_int <- getwsDatosDiarios(idsesion,cestacion,cvariable,fecha)

# CONSULTAR DATOS DIARIOS RAW MULTI (UNA ESTACION, VARIAS VARIABLES, VARIAS FECHAS)
pkests <- '183'
pkvars <- c('2','711')
pkfecs <- c('100741','100742')
result_diarios_raw1 <- getwsDatosDiariosMultiRaw(idsesion,pkests,pkvars,pkfecs)

# CONSULTAR DATOS DIARIOS INT MULTI (UNA ESTACION, VARIAS VARIABLES, VARIAS FECHAS)
cestaciones <- 'EARM22'
cvariables <- c('TD1','PD23')
fechas <- c('26/10/2025','27/10/2025')
result_diarios_int1 <- getwsDatosDiariosMulti(idsesion,cestaciones,cvariables,fechas)

# CONSULTA DE DATOS DIARIOS RAW MULTI (VARIAS ESTACIONES, UNA VARIABLE, VARIAS FECHAS)
cestaciones <- c('EARM22','SIVA40')
cvariables <- c('TD1')
fechas <- c('25/10/2025','26/10/2025','27/10/2025','28/10/2025')
result_diarios_int2 <- getwsDatosDiariosMulti(idsesion,cestaciones,cvariables,fechas)

# CONSULTA DE DATOS DIARIOS RAW MULTI (VARIAS ESTACIONES, VARIAS VARIABLES, UNA FECHA)
cestaciones <- c('EARM22','SIVA40')
cvariables <- c('TD1','PD23')
fechas <- c('25/10/2025')
result_diarios_int3 <- getwsDatosDiariosMulti(idsesion,cestaciones,cvariables,fechas)

# CONSULTA DE DATOS DIARIOS EN UN INTERVALO DE FECHAS RAW
pkests <- c('183','143')
pkvars <- c('2','711') # TD1 y PD23
pkfec_ini <- '100730' # 15/10/2025
pkfec_fin <- '100741' # 26/10/2025
result_diarios_intervalo_raw2 <- getwsDatosDiarios2Raw(idsesion,pkests,pkvars,pkfec_ini,pkfec_fin)

# CONSULTA DE DATOS DIARIOS EN UN INTERVALO DE FECHAS INT
cestaciones <- c('EARM22','SIVA40')
cvariables <- c('TD1','PD23')
fecha_ini <- '01/10/2025'
fecha_fin <- '31/10/2025'
result_diarios_intervalo_int <- getwsDatosDiarios2(idsesion,cestaciones,cvariables,fecha_ini,fecha_fin)

# CONSULTA DE DATOS DIARIOS EN UN INTERVALO DE FECHAS INT
cestaciones <- c('EARM22','EARM01')
cvariables <- c('TD1','PD23')
fecha_ini <- '01/10/2025'
fecha_fin <- '31/10/2025'
result_diarios_intervalo_int2 <- getwsDatosDiarios2(idsesion,cestaciones,cvariables,fecha_ini,fecha_fin)

# CONSULTA DE DATOS MENSUALES RAW
pkests <- c('183') # EARM22
pkvars <- c('22') # TM1
pkfecs <- c('98981') # 31/12/2020
datos_mensuales_raw <- getwsDatosMensualesRaw(idsesion,pkests,pkvars,pkfecs)

# CONSULTA DE DATOS MENSUALES INT
cestaciones <- c('EARM22')
cvariables <- c('TM1','TM2','TM3')
fechas <- c('30/11/2020')
datos_mensuales_int <- getwsDatosMensuales(idsesion,cestaciones,cvariables,fechas)
