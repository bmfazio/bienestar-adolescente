ece_load <- drake_plan(
  ece_ready =
    rbind(
      read_xlsx(
        file_in("MINEDUDIR__/ece/EvaluacionCensal_Secundaria_SEGUNDO_14112018_160622.xlsx"),
        range = "J9:J10") %>%
        cbind(materia = "READ"),
      read_xlsx(
        file_in("MINEDUDIR__/ece/EvaluacionCensal_Secundaria_SEGUNDO_14112018_160622.xlsx"),
        range = "J286:J287") %>%
        cbind(materia = "MATH")
    ) %>%
    mutate(Satisfactorio = as.numeric(
      stri_replace_all_fixed(Satisfactorio, pattern = "%", replacement = "")))
) %>%
  evaluate_plan(
    rules = list(MINEDUDIR__ = minedudir),
    expand = FALSE
  )