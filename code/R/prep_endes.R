endes_load <- drake_plan(
  # Household basic data
  rech0_2013 =
    rech0_import(file_in("INEIDIR__/endes/2013/407-Modulo64/RECH0.SAV"), 2013),
  rech0_2014 =
    rech0_import(file_in("INEIDIR__/endes/2014/441-Modulo64/RECH0.SAV"), 2014),
  rech0_2015 =
    rech0_import(file_in("INEIDIR__/endes/2015/504-Modulo64/RECH0.SAV"), 2015),
  rech0_2016 =
    rech0_import(file_in("INEIDIR__/endes/2016/548-Modulo64/RECH0.SAV"), 2016),
  rech0_2017 =
    rech0_import(file_in("INEIDIR__/endes/2017/Modulo64/RECH0.SAV"), 2017),
  rech0 = rbind(rech0_2013, rech0_2014, rech0_2015, rech0_2016, rech0_2017),
  # Respondent basic data
  rec0111_2013 =
    rec0111_import(file_in("INEIDIR__/endes/2013/407-Modulo66/REC0111.SAV"), 2013),
  rec0111_2014 =
    rec0111_import(file_in("INEIDIR__/endes/2014/441-Modulo66/REC0111.SAV"), 2014),
  rec0111_2015 =
    rec0111_import(file_in("INEIDIR__/endes/2015/504-Modulo66/REC0111.SAV"), 2015),
  rec0111_2016 =
    rec0111_import(file_in("INEIDIR__/endes/2016/548-Modulo66/REC0111.SAV"), 2016),
  rec0111_2017 =
    rec0111_import(file_in("INEIDIR__/endes/2017/Modulo66/REC0111.SAV"), 2017),
  rec0111 = rbind(rec0111_2013, rec0111_2014, rec0111_2015, rec0111_2016, rec0111_2017),
  # Household data
  rech23_2013 =
    rech23_import(file_in("INEIDIR__/endes/2013/407-Modulo65/RECH23.SAV"), 2013),
  rech23_2014 =
    rech23_import(file_in("INEIDIR__/endes/2014/441-Modulo65/RECH23.SAV"), 2014),
  rech23_2015 =
    rech23_import(file_in("INEIDIR__/endes/2015/504-Modulo65/RECH23.SAV"), 2015),
  rech23_2016 =
    rech23_import(file_in("INEIDIR__/endes/2016/548-Modulo65/RECH23.SAV"), 2016),
  rech23_2017 =
    rech23_import(file_in("INEIDIR__/endes/2017/Modulo65/RECH23.SAV"), 2017),
  rech23 = rbind(rech23_2013, rech23_2014, rech23_2015, rech23_2016, rech23_2017),
  re223132_2013 =
    re223132_import(file_in("INEIDIR__/endes/2013/407-Modulo67/RE223132.SAV"), 2013),
  re223132_2014 =
    re223132_import(file_in("INEIDIR__/endes/2014/441-Modulo67/RE223132.SAV"), 2014),
  re223132_2015 =
    re223132_import(file_in("INEIDIR__/endes/2015/504-Modulo67/RE223132.SAV"), 2015),
  re223132_2016 =
    re223132_import(file_in("INEIDIR__/endes/2016/548-Modulo67/RE223132.SAV"), 2016),
  re223132_2017 =
    re223132_import(file_in("INEIDIR__/endes/2017/Modulo67/RE223132.SAV"), 2017),
  re223132 = rbind(re223132_2013, re223132_2014, re223132_2015, re223132_2016, re223132_2017),
  re516171_2013 =
    re516171_import(file_in("INEIDIR__/endes/2013/407-Modulo71/RE516171.SAV"), 2013),
  re516171_2014 =
    re516171_import(file_in("INEIDIR__/endes/2014/441-Modulo71/RE516171.SAV"), 2014),
  re516171_2015 =
    re516171_import(file_in("INEIDIR__/endes/2015/504-Modulo71/RE516171.SAV"), 2015),
  re516171_2016 =
    re516171_import(file_in("INEIDIR__/endes/2016/548-Modulo71/RE516171.SAV"), 2016),
  re516171_2017 =
    re516171_import(file_in("INEIDIR__/endes/2017/Modulo71/RE516171.SAV"), 2017),
  re516171 = rbind(re516171_2013, re516171_2014, re516171_2015, re516171_2016, re516171_2017),
  # Cuestionario de Salud
  csalud01_2013 =
    csalud01_import(file_in("INEIDIR__/endes/2013/407-Modulo414/CSALUD01.sav"), 2013),
  csalud01_2014 =
    csalud01_import(file_in("INEIDIR__/endes/2014/441-Modulo414/CSALUD01.sav"), 2014),
  csalud01_2015 =
    csalud01_import(file_in("INEIDIR__/endes/2015/504-Modulo414/CSALUD01.sav"), 2015),
  csalud01_2016 =
    csalud01_import(file_in("INEIDIR__/endes/2016/548-Modulo414/CSALUD01.sav"), 2016),
  csalud01_2017 =
    csalud01_import(file_in("INEIDIR__/endes/2017/Modulo414/CSALUD01.sav"), 2017),
  csalud01 = rbind(csalud01_2013, csalud01_2014, csalud01_2015, csalud01_2016, csalud01_2017),
  # Anemia mujer
  rec42_2013 =
    rec42_import(file_in("INEIDIR__/endes/2013/407-Modulo70/REC42.SAV"), 2013),
  rec42_2014 =
    rec42_import(file_in("INEIDIR__/endes/2014/441-Modulo70/REC42.SAV"), 2014),
  rec42_2015 =
    rec42_import(file_in("INEIDIR__/endes/2015/504-Modulo70/REC42.SAV"), 2015),
  rec42_2016 =
    rec42_import(file_in("INEIDIR__/endes/2016/548-Modulo70/REC42.SAV"), 2016),
  rec42_2017 =
    rec42_import(file_in("INEIDIR__/endes/2017/Modulo70/REC42.SAV"), 2017),
  rec42 = rbind(rec42_2013, rec42_2014, rec42_2015, rec42_2016, rec42_2017),
  # Violencia domestica
  rec84dv_2013 =
    rec84dv_import(file_in("INEIDIR__/endes/2013/407-Modulo73/REC84DV.SAV"), 2013),
  rec84dv_2014 =
    rec84dv_import(file_in("INEIDIR__/endes/2014/441-Modulo73/REC84DV.SAV"), 2014),
  rec84dv_2015 =
    rec84dv_import(file_in("INEIDIR__/endes/2015/504-Modulo73/REC84DV.SAV"), 2015),
  rec84dv_2016 =
    rec84dv_import(file_in("INEIDIR__/endes/2016/548-Modulo73/REC84DV.SAV"), 2016),
  rec84dv_2017 =
    rec84dv_import(file_in("INEIDIR__/endes/2017/Modulo73/REC84DV.SAV"), 2017),
  rec84dv = rbind(rec84dv_2013, rec84dv_2014, rec84dv_2015, rec84dv_2016, rec84dv_2017)
) %>%
  evaluate_plan(
    rules = list(INEIDIR__ = ineidir),
    expand = FALSE
)

endes_merge <- drake_plan(
  # Cuestionarios exclusivos para mujer
  endes_mujer = (
    rec0111 %>%
      left_join(rech23, by = "HHID") %>%
      left_join(re223132, by = "CASEID") %>%
      left_join(re516171, by = "CASEID") %>%
      left_join(re758081, by = "CASEID") %>%
      left_join(rec42, by = "CASEID") %>%
      left_join(rec84dv, by = "CASEID")
    ) %>%
    filter(!is.na(V005)) %>%
    mutate(
      psuid = V021,
      peso = V005/10**6,
      region = label_vals(SHREGION, "3regiones"),
      estrato.region = putlabel(V023),
      estrato.urbrur = V022,
      area = case_when(V025 == 1 ~ "URBANA", V025 == 2 ~ "RURAL"),
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
    ) %>%
    mutate(
      region = case_when(
        estrato.region == "Lima" & region == "Lima metropolitana" ~ "LIMA PROVINCIA",
        estrato.region == "Lima" & region != "Lima metropolitana" ~ "LIMA REGION",
        TRUE ~ estrato.region
        )
      ) %>%
    svydesign(ids =~ psuid,
              strata =~ estrato.region + estrato.urbrur,
              weights =~ peso,
              data = .),
  # Cuestionarios de salud
  endes_salud = (
    csalud01 %>%
      left_join(rech23, by = "HHID") %>%
      left_join(rech0, by = "HHID")
    ) %>%
    filter(!is.na(PESO15_AMAS)) %>%
    mutate(
      psuid = HV021,
      peso = PESO15_AMAS/10**6,
      region = putlabel(SHREGION),
      area = case_when(HV025 == 1 ~ "URBANA", HV025 == 2 ~ "RURAL"),
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
      ) %>%
    mutate(
      region = case_when(
        estrato.region == "Lima" & region == "Lima metropolitana" ~ "LIMA PROVINCIA",
        estrato.region == "Lima" & region != "Lima metropolitana" ~ "LIMA REGION",
        TRUE ~ estrato.region
        )
      ) %>%
    svydesign(ids =~ psuid,
              strata =~ estrato.region + estrato.urbrur,
              weights =~ peso,
              data = .)
)