endes.rech0_plan <- drake_plan(
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
  # Household basic data
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
  rec0111 = rbind(rec0111_2013, rec0111_2014, rec0111_2015, rec0111_2016, rec0111_2017)
) %>%
  evaluate_plan(
    rules = list(INEIDIR__ = ineidir),
    expand = FALSE
  )
  # Household data
  rech23 =
    import(
      file_in("INEIDIR__/endes/2017/Modulo65/RECH23.SAV"),
      setclass = "data.table"),
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