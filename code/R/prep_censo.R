censo_load <- drake_plan(
  censo =
    fread(
      file_in("DATADIR__/inei/censo/2017/censo_full.csv")),
  censo_desag_11a17 = censo_edad(censo, 11, 17),
  censo_desag_12a17 = censo_edad(censo, 12, 17),
  censo_desag_18a24 = censo_edad(censo, 18, 24)
) %>%
  evaluate_plan(
    rules = list(DATADIR__ = datadir),
    expand = FALSE
  )