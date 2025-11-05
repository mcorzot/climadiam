# Declarar variables globales para evitar warnings de R CMD check
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(
      "minutos_horas"
    )
  )
}
