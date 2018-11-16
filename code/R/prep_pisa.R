pisa_load <- drake_plan(
  pisa.flt =
    fread(
      file_in("DATADIR__/pisa/2015/cy6_ms_cmb_stu_flt_PER.csv")),
  pisa.qqq =
    fread(
      file_in("DATADIR__/pisa/2015/CY6_MS_CMB_STU_QQQ_PER.csv"))
) %>%
  evaluate_plan(
    rules = list(DATADIR__ = datadir),
    expand = FALSE
  )

### falta ver como aplicar los pesos
  # revisar la sintaxis de intsvy