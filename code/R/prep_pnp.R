pnp_load <- drake_plan(
  pnp.violencia =
    fread(
      file_in("DATADIR__/pnp/observatorio_violencia.csv")) %>%
    mutate(Edad = Edad %>% substr(1,2) %>% as.numeric)
) %>%
  evaluate_plan(
    rules = list(DATADIR__ = datadir),
    expand = FALSE
  )

# pisa_merge <- drake_plan(
#   pisa.finlit =
#     pisa.qqq %>%
#     select(sexo = ST004D01T,
#            W_FSTUWT, CNTSTUID,
#            paste0("W_FSTURWT",1:80)) %>%
#     right_join(pisa.flt,
#                by = "CNTSTUID"),
#   pisa.satisf =
#     pisa.qqq %>%
#     select(sexo = ST004D01T,
#            W_FSTUWT, CNTSTUID,
#            paste0("W_FSTURWT",1:80)) %>%
#     right_join(pisa.qq2 %>% filter(CNT == "PER"),
#                by = "CNTSTUID")
# )
# 
# pisa_plan <- bind_plans(
#   pisa_load,
#   pisa_merge
# )