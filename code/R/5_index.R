plan_index <- drake_plan(
  tabla_indice =
    data.frame(
      dimensiones = c(
        "Salud",
        "Educación",
        "Seguridad",
        "Trabajo",
        "Participación",
        "ÍNDICE GLOBAL"
      ),
      indice = c(
        mean(tabla_salud$normalized),
        mean(tabla_educacion$normalized),
        mean(tabla_seguridad$normalized),
        mean(tabla_trabajo$normalized),
        mean(tabla_participacion$normalized),
        0
      )
    ) %>%
    (function(x){x[6,2] <- exp(mean(log(x[-6,2])));x})
)

# tabla_salud
# tabla_educacion
# tabla_seguridad
# tabla_trabajo
# tabla_participacion