# climadiam

**Paquete R para la consulta de datos del Subsistema de Climatología Ambiental (CLIMA)**  
de la **Red de Información Ambiental de Andalucía (REDIAM)**.

Este paquete permite conectarse mediante servicios web a la base de datos del Subsistema CLIMA, 
obteniendo información meteorológica y ambiental de Andalucía.

---

## 🚀 Instalación

```r
# Instalar el paquete 'remotes' si no lo tienes
install.packages("remotes")

# Instalar el paquete directamente desde GitHub
remotes::install_github("mcorzot/climadiam")
```

---

## 📦 Carga del paquete

```r
library(climadiam)
```

---

## 🔑 Usuario y contraseña de acceso libre

```r
user <- "usuario"
password <- "usuario"
```

---

## 🔑 Solicitud de usuario avanzado

Para usuarios que realizan trabajos para la Junta de Andalucía y entidades asociadas a la Red de Información Ambiental de Andalucía. Permite el acceso a todos los datos integrados incluyendo los de la Agencia Estatal de Meteorología. La autorización para ser registrado como usuario avanzado debe solicitarse a la **Consejería de Sostenibilidad y Medio Ambiente** en el siguiente enlace: [Solicitud de información Ambiental](https://www.juntadeandalucia.es/medioambiente/portal/web/cae/detalle/-/asset_publisher/S16tVNj0etAT/content/solicitud-de-informaci-c3-b3n-ambiental/20151).

---
## 🧭 Obtener el ID de sesión

```r
idsesion <- getwsIDSesion(user, password)
```

---

## 📚 Tablas maestras

```r
comarcas            <- getwsComarcasList(idsesion)
areas_geograficas   <- getwsAreasGeograficasList(idsesion)
areas_climaticas    <- getwsAreasClimaticasList(idsesion)
escalas             <- getwsEscalasList(idsesion)
gestores            <- getwsGestoresList(idsesion)
redes               <- getwsRedesList(idsesion)
magnitudes          <- getwsMagnitudesList(idsesion)
unidades            <- getwsUnidadesList(idsesion)
provincias          <- getwsProvinciasList(idsesion)
municipios          <- getwsMunicipiosList(idsesion)
estaciones          <- getwsEstacionesList(idsesion)
estados_datos       <- getwsEstadosDatosList(idsesion)
estaciones_detalle  <- getwsEstacionesExpandidoList(idsesion)
tipos_medias        <- getwsTiposMediasList(idsesion)
variables           <- getwsVariablesList(idsesion)
fechas              <- getwsFechasList(idsesion)
```

---

## 📅 Consultas de ejemplo

### Consulta de fecha concreta
```r
date  <- format(as.Date(Sys.Date() - 4, format = "%Y-%m-%d"), format = "%d/%m/%Y")
fecha <- getwsFechas(as.character(date), idsesion)
```

---

### Variables por estación concreta (RAW)
```r
varpkvar <- "1"
variablesestacioneslistparams_raw <- getwsVariablesEstacionesListRaw(varpkvar, idsesion)
```

### Variables por estación concreta (INT)
```r
cvariable <- "TI1"
variablesestacioneslistparams_int <- getwsVariablesEstacionesList(cvariable, idsesion)
```

---

### Existencia de datos por estación (RAW)
```r
pkest <- "183"
existencias_estacion_raw <- getwsDatosExisteEstacionesListRaw(idsesion, pkest)
```

### Existencia de datos por estación (INT)
```r
cestacion <- "EARM22"
existencias_estacion_int <- getwsDatosExisteEstacionesList(idsesion, cestacion)
```

---

## 🌤️ Consultas de datos diarios

### Una estación, una variable, una fecha (RAW)
```r
pkest <- "183"  # EARM22
pkvar <- "2"    # TD1
pkfec <- "100741"
result_diaria_raw <- getwsDatosDiariosRaw(idsesion, pkest, pkvar, pkfec)
```

### Una estación, una variable, una fecha (INT)
```r
cestacion <- "EARM22"
cvariable <- "TD1"
fecha <- "26/10/2025"
result_diaria_int <- getwsDatosDiarios(idsesion, cestacion, cvariable, fecha)
```

---

### Una estación, varias variables, varias fechas (RAW)
```r
pkests <- "183"
pkvars <- c("2", "711")
pkfecs <- c("100741", "100742")
result_diarios_raw1 <- getwsDatosDiariosMultiRaw(idsesion, pkests, pkvars, pkfecs)
```

### Una estación, varias variables, varias fechas (INT)
```r
cestaciones <- "EARM22"
cvariables  <- c("TD1", "PD23")
fechas      <- c("26/10/2025", "27/10/2025")
result_diarios_int1 <- getwsDatosDiariosMulti(idsesion, cestaciones, cvariables, fechas)
```

---

### Varias estaciones, una variable, varias fechas
```r
cestaciones <- c("EARM22", "SIVA40")
cvariables  <- c("TD1")
fechas      <- c("25/10/2025", "26/10/2025", "27/10/2025", "28/10/2025")
result_diarios_int2 <- getwsDatosDiariosMulti(idsesion, cestaciones, cvariables, fechas)
```

### Varias estaciones, varias variables, una fecha
```r
cestaciones <- c("EARM22", "SIVA40")
cvariables  <- c("TD1", "PD23")
fechas      <- c("25/10/2025")
result_diarios_int3 <- getwsDatosDiariosMulti(idsesion, cestaciones, cvariables, fechas)
```

---

## 📆 Consultas de intervalos de fechas

### Datos diarios (RAW)
```r
pkests     <- c("183", "143")
pkvars     <- c("2", "711")     # TD1 y PD23
pkfec_ini  <- "100730"          # 15/10/2025
pkfec_fin  <- "100741"          # 26/10/2025
result_diarios_intervalo_raw2 <- getwsDatosDiarios2Raw(idsesion, pkests, pkvars, pkfec_ini, pkfec_fin)
```

### Datos diarios (INT)
```r
cestaciones <- c("EARM22", "SIVA40")
cvariables  <- c("TD1", "PD23")
fecha_ini   <- "01/10/2025"
fecha_fin   <- "31/10/2025"
result_diarios_intervalo_int <- getwsDatosDiarios2(idsesion, cestaciones, cvariables, fecha_ini, fecha_fin)
```

---

## 📅 Consultas de datos mensuales

### Datos mensuales (RAW)
```r
pkests <- c("183")     # EARM22
pkvars <- c("22")      # TM1
pkfecs <- c("98981")   # 31/12/2020
datos_mensuales_raw <- getwsDatosMensualesRaw(idsesion, pkests, pkvars, pkfecs)
```

### Datos mensuales (INT)
```r
cestaciones <- c("EARM22")
cvariables  <- c("TM1", "TM2", "TM3")
fechas      <- c("30/11/2020")
datos_mensuales_int <- getwsDatosMensuales(idsesion, cestaciones, cvariables, fechas)
```

---

## 🧪 Ejemplo práctico de uso

A continuación se muestra un ejemplo completo que ilustra cómo, a partir de una selección de variables, se pueden obtener los **datos intradiarios de ayer** y los **datos diarios de antes de ayer** de todas las estaciones disponibles.

📄 [GitHub - climadiam.Rmd](examples/climadiam.Rmd)
📄 [Ver el ejemplo en RPubs](https://rpubs.com/mcorzot/climadiam)

---

## 📖 Autoría

Desarrollado por la **Agencia de Medio Ambiente y Agua de Andalucía**  
para el acceso a los servicios web de la **REDIAM – Subsistema CLIMA**.

Repositorio oficial: [https://github.com/mcorzot/climadiam](https://github.com/mcorzot/climadiam)
