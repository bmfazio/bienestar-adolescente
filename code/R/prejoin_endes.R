endes5_plan <- drake_plan(
  # Household basic data
  rech0_2013 =
    rech0_import(file_in("INEIDIR__/endes/2013/407-Modulo64/RECH0.SAV")),
  rech0_2014 =
    rech0_import(file_in("INEIDIR__/endes/2014/441-Modulo64/RECH0.SAV")),
  rech0_2015 =
    rech0_import(file_in("INEIDIR__/endes/2015/504-Modulo64/RECH0.SAV")),
  rech0_2016 =
    rech0_import(file_in("INEIDIR__/endes/2016/548-Modulo64/RECH0.SAV")),
  rech0_2017 =
    rech0_import(file_in("INEIDIR__/endes/2017/Modulo64/RECH0.SAV")),
  rech0 = rbind(rech0_2013, rech0_2014, rech0_2015, rech0_2016, rech0_2017),
  # Respondent basic data
  rec0111_2013 =
    rec0111_import(file_in("INEIDIR__/endes/2013/407-Modulo66/REC0111.SAV")),
  rec0111_2014 =
    rec0111_import(file_in("INEIDIR__/endes/2014/441-Modulo66/REC0111.SAV")),
  rec0111_2015 =
    rec0111_import(file_in("INEIDIR__/endes/2015/504-Modulo66/REC0111.SAV")),
  rec0111_2016 =
    rec0111_import(file_in("INEIDIR__/endes/2016/548-Modulo66/REC0111.SAV")),
  rec0111_2017 =
    rec0111_import(file_in("INEIDIR__/endes/2017/Modulo66/REC0111.SAV")),
  rec0111 = rbind(rec0111_2013, rec0111_2014, rec0111_2015, rec0111_2016, rec0111_2017),
  # Household data
  rech23_2013 =
    rech23_import(file_in("INEIDIR__/endes/2013/407-Modulo65/RECH23.SAV")),
  rech23_2014 =
    rech23_import(file_in("INEIDIR__/endes/2014/441-Modulo65/RECH23.SAV")),
  rech23_2015 =
    rech23_import(file_in("INEIDIR__/endes/2015/504-Modulo65/RECH23.SAV")),
  rech23_2016 =
    rech23_import(file_in("INEIDIR__/endes/2016/548-Modulo65/RECH23.SAV")),
  rech23_2017 =
    rech23_import(file_in("INEIDIR__/endes/2017/Modulo65/RECH23.SAV")),
  rech23 = rbind(rech23_2013, rech23_2014, rech23_2015, rech23_2016, rech23_2017),
  re223132_2013 =
    re223132_import(file_in("INEIDIR__/endes/2013/407-Modulo67/RE223132.SAV")),
  re223132_2014 =
    re223132_import(file_in("INEIDIR__/endes/2014/441-Modulo67/RE223132.SAV")),
  re223132_2015 =
    re223132_import(file_in("INEIDIR__/endes/2015/504-Modulo67/RE223132.SAV")),
  re223132_2016 =
    re223132_import(file_in("INEIDIR__/endes/2016/548-Modulo67/RE223132.SAV")),
  re223132_2017 =
    re223132_import(file_in("INEIDIR__/endes/2017/Modulo67/RE223132.SAV")),
  re223132 = rbind(re223132_2013, re223132_2014, re223132_2015, re223132_2016, re223132_2017),
  re516171_2013 =
    re516171_import(file_in("INEIDIR__/endes/2013/407-Modulo71/RE516171.SAV")),
  re516171_2014 =
    re516171_import(file_in("INEIDIR__/endes/2014/441-Modulo71/RE516171.SAV")),
  re516171_2015 =
    re516171_import(file_in("INEIDIR__/endes/2015/504-Modulo71/RE516171.SAV")),
  re516171_2016 =
    re516171_import(file_in("INEIDIR__/endes/2016/548-Modulo71/RE516171.SAV")),
  re516171_2017 =
    re516171_import(file_in("INEIDIR__/endes/2017/Modulo71/RE516171.SAV")),
  re516171 = rbind(re516171_2013, re516171_2014, re516171_2015, re516171_2016, re516171_2017),
  # Cuestionario de Salud
  csalud01_2013 =
    csalud01_import(file_in("INEIDIR__/endes/2013/407-Modulo414/CSALUD01.sav")),
  csalud01_2014 =
    csalud01_import(file_in("INEIDIR__/endes/2014/441-Modulo414/CSALUD01.sav")),
  csalud01_2015 =
    csalud01_import(file_in("INEIDIR__/endes/2015/504-Modulo414/CSALUD01.sav")),
  csalud01_2016 =
    csalud01_import(file_in("INEIDIR__/endes/2016/548-Modulo414/CSALUD01.sav")),
  csalud01_2017 =
    csalud01_import(file_in("INEIDIR__/endes/2017/Modulo414/CSALUD01.sav")),
  csalud01 = rbind(csalud01_2013, csalud01_2014, csalud01_2015, csalud01_2016, csalud01_2017)
) %>%
  evaluate_plan(
    rules = list(INEIDIR__ = ineidir),
    expand = FALSE
)

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