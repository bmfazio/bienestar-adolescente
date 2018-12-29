pisa_load <- drake_plan(
  pisa.flt =
    fread(
      file_in("DATADIR__/pisa/2015/cy6_ms_cmb_stu_flt_PER.csv")),
  pisa.qqq =
    fread(
      file_in("DATADIR__/pisa/2015/CY6_MS_CMB_STU_QQQ_PER.csv")),
  pisa.qq2 =
    fread(
      file_in("DATADIR__/pisa/2015/CY6_MS_CMB_STU_QQ2_PER.csv"))
) %>%
  evaluate_plan(
    rules = list(DATADIR__ = datadir),
    expand = FALSE
  )

pisa_merge <- drake_plan(
  pisa.finlit =
    pisa.qqq %>%
    select(sexo = ST004D01T,
           W_FSTUWT, CNTSTUID,
           paste0("W_FSTURWT",1:80)) %>%
    right_join(pisa.flt,
               by = "CNTSTUID"),
  pisa.satisf =
    pisa.qqq %>%
    select(sexo = ST004D01T,
           W_FSTUWT, CNTSTUID,
           paste0("W_FSTURWT",1:80)) %>%
    right_join(pisa.qq2 %>% filter(CNT == "PER"),
               by = "CNTSTUID")
)

pisa_plan <- bind_plans(
  pisa_load,
  pisa_merge
)