iccs_load <- drake_plan(
  iccs.isa =
    haven::read_sav(
      file_in("DATADIR__/iccs/2016/Data/ISAPERC3.sav")),
  iccs.isg =
    haven::read_sav(
      file_in("DATADIR__/iccs/2016/Data/ISGPERC3.sav")),
  iccs = iccs.isa %>%
    left_join(iccs.isg, by = c("IDSTUD", "S_GENDER", "S_AGE", "TOTWGTS")) %>%
    filter(S_AGE < 18) %>%
    mutate(sexo = ifelse(S_GENDER==0, "HOMBRE", "MUJER"),
           confianza_gob = IS3G26A,
           opinion_cole = IS3G17B) %>%
    svrepdesign(repweights = "SRWGT[0-9]+.x", weights = ~TOTWGTS, type = "JK1",
                data = .)
) %>%
  evaluate_plan(
    rules = list(DATADIR__ = datadir),
    expand = FALSE
  )