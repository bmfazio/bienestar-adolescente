plan_index <- drake_plan(
  
  indices_dimension =
    tabla_normalizada %>%
    group_by(desag, dimension) %>%
    summarize(indice = mean(norm)),
  
  indices_final =
    indices_dimension %>%
    group_by(desag) %>%
    summarize(indice = exp(mean(log(indice)))) %>%
    cbind(dimension = "GLOBAL") %>%
    bind_rows(indices_dimension) %>%
    arrange(
      desag,
      match(dimension,
            c("SALUD",
              "EDUCACION",
              "SEGURIDAD",
              "TRABAJO",
              "PARTICIPACION",
              "GLOBAL"))
      )
)