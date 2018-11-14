ece_load <- drake_plan(
  ece =
    rbind(
      read_xlsx(
        file_in("MINEDUDIR__/ece/EvaluacionCensal_Secundaria_SEGUNDO_14112018_160622.xlsx"),
        range = "J9:J10") %>%
        cbind(materia = "lectura"),
      read_xlsx(
        file_in("MINEDUDIR__/ece/EvaluacionCensal_Secundaria_SEGUNDO_14112018_160622.xlsx"),
        range = "J286:J287") %>%
        cbind(materia = "matematica")
    )
) %>%
  evaluate_plan(
    rules = list(MINEDUDIR__ = minedudir),
    expand = FALSE
  )