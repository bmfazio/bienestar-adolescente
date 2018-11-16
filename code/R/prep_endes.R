endes_load <- drake_plan(
  # Household basic data
  rech0 =
    import(
      file_in("INEIDIR__/endes/2017/Modulo64/RECH0.SAV"),
      setclass = "data.table"),
  # Respondent basic data
  rec0111 =
    import(
      file_in("INEIDIR__/endes/2017/Modulo66/REC0111.SAV"),
      setclass = "data.table"),
  # Reproduction/Contraception
  # rec21 =
  #   import(
  #     file_in("INEIDIR__/endes/2017/Modulo67/REC21.SAV"),
  #     setclass = "data.table"),
  re223132 =
    import(
      file_in("INEIDIR__/endes/2017/Modulo67/RE223132.SAV"),
      setclass = "data.table"),
  re516171 =
    import(
      file_in("INEIDIR__/endes/2017/Modulo71/RE516171.SAV"),
      setclass = "data.table"),
  re758081 =
    import(
      file_in("INEIDIR__/endes/2017/Modulo72/RE758081.SAV"),
      setclass = "data.table"),
  # Cuestionario de Salud
  csalud01 =
    data.table(
      haven::read_sav(
        file_in("INEIDIR__/endes/2017/Modulo414/CSALUD01.sav"),
        encoding = "latin1")),
  # Cuestionario de Salud (<12a)
  csalud08 =
    data.table(
      haven::read_sav(
        file_in("INEIDIR__/endes/2017/Modulo414/CSALUD08.sav"),
        encoding = "latin1")),
  # Anemia mujer
  rec42 =
    import(
      file_in("INEIDIR__/endes/2017/Modulo70/REC42.SAV"),
      setclass = "data.table"),
  # Mortalidad materna
  # rec83 =
  #   import(
  #     file_in("INEIDIR__/endes/2017/Modulo73/REC83.SAV"),
  #     setclass = "data.table"),
  # Violencia domestica
  rec84dv =
    import(
      file_in("INEIDIR__/endes/2017/Modulo73/REC84DV.SAV"),
      setclass = "data.table")
) %>%
  evaluate_plan(
    rules = list(INEIDIR__ = ineidir),
    expand = FALSE
  )
  
endes_merge <- drake_plan(
  # Cuestionarios exclusivos para mujer
  endes_mujer = (
    rec0111 %>%
      merge(re223132, by = "CASEID", all.x = T) %>%
      merge(re516171, by = "CASEID", all.x = T) %>%
      merge(re758081, by = "CASEID", all.x = T) %>%
      merge(rec42, by = "CASEID", all.x = T) %>%
      merge(rec84dv, by = "CASEID", all.x = T)
    )[,.(
      psuid = V021,
      peso = V005/10**6,
      estrato.region = putlabel(V023),
      estrato.urbrur = V022,
      gedad = putlabel(V013),
      inisex = V525,
      nhijos = V209,
      antic.moderno = as.numeric(V364==1),
      antic.modotra = as.numeric(V364<=2),
      ultsex.condon =
        case_when(
          V761 == 8 ~ NA_real_,
          TRUE ~ V761),
      v.emoc = D104,
      v.fisi = D106,
      v.fisigrav = D107,
      v.sex = D108,
      edad.matri = V511,
      anemia = as.numeric(V457<4),
      imc = ifelse(V445 == 9998, NA, V445)
      )] %>%
    svydesign(ids =~ psuid,
              strata =~ estrato.region + estrato.urbrur,
              weights =~ peso,
              data = .),
  # Cuestionarios de salud
  endes_salud = (
    csalud01 %>%
      merge(rech0, by = "HHID", all.x = TRUE)
    )[!is.na(PESO15_AMAS), .(
      psuid = HV021,
      peso = PESO15_AMAS/10**6,
      estrato.region = putlabel(HV023),
      estrato.urbrur = HV022,
      sexo = putlabel(QSSEXO),
      edad = QS23,
      alc.vida =
        case_when(
          QS206 == 1 ~ 1,
          QS206 == 2 ~ 0,
          TRUE ~ NA_real_),
      alc.anho =
        case_when(
          QS208 == 1 ~ 1,
          QS206 == 2 | QS208 == 2 ~ 0,
          TRUE ~ NA_real_),
      alc.12va =
        case_when(
          QS209 == 1 ~ 1,
          QS206 == 2 | QS208 == 2 | QS209 == 2 ~ 0,
          TRUE ~ NA_real_),
      alc.30d =
        case_when(
          QS210 == 1 ~ 1,
          QS206 == 2 | QS208 == 2 | QS209 == 2 | QS210 == 2 ~ 0,
          TRUE ~ NA_real_),
      tabaco.anho =
        case_when(
          QS200 == 1 ~ 1,
          QS200 == 2 ~ 0,
          TRUE ~ NA_real_),
      tabaco.30d =
        case_when(
          QS201 == 1 ~ 1,
          QS200 == 2 | QS201 == 2 ~ 0,
          TRUE ~ NA_real_),
      v.golpe = as.numeric(QS710>1),
      v.arma = as.numeric(QS711>1)
      )] %>%
    svydesign(ids =~ psuid,
              strata =~ estrato.region + estrato.urbrur,
              weights =~ peso,
              data = .)
)