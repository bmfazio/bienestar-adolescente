enares_load <- drake_plan(
  enares200 =
    import(
      file_in("INEIDIR__/enares/503-Modulo770/14_CRS04_CAP200.sav"),
      setclass = "data.table"),
  enares_ready =
    enares200[,.(
      region =
        DIREED %>%
        decode_direed %>%
        ifelse(. == "LIMA PROVINCIAS", "LIMA REGION", .),
      psu1 = as.numeric(factor(COD_MOD)),
      psu2 = as.numeric(factor(paste(COD_MOD, C3ANIO, TURNO, C3SECC))),
      peso = Factor_Alumnos,
      area = toupper(putlabel(AREA)),
      sexo = ifelse(SEXO == 1, "MUJER", "HOMBRE"),
      edad = EDAD,
      casa.v.emo =as.numeric(
        mapply(function(...)2*length(list(...))-sum(...),
               C4P201_1,C4P201_2,C4P201_3,C4P201_4,C4P201_5,
               C4P201_6,C4P201_7,C4P201_8,C4P201_9,C4P201_10,
               C4P201_11)>0),
      casa.v.fis = as.numeric(
        mapply(function(...)2*length(list(...))-sum(...),
               C4P205_1,C4P205_2,C4P205_3,C4P205_4,C4P205_5,
               C4P205_6,C4P205_7)>0),
      cole.v.emo = as.numeric(
        mapply(function(...)2*length(list(...))-sum(...),
               C4P225_1,C4P225_2,C4P225_3,C4P225_4,C4P225_5,
               C4P225_6,C4P225_7,C4P225_8,C4P225_9,C4P225_10,
               C4P225_11,C4P225_12,C4P225_13,C4P225_14)>0),
      cole.v.fis = as.numeric(
        mapply(function(...)2*length(list(...))-sum(...),
               C4P229_1,C4P229_2,C4P229_3,C4P229_4,C4P229_5,
               C4P229_6,C4P229_7,C4P229_8,C4P229_9,C4P229_10)>0),
      v.sex = as.numeric(
        mapply(function(...)2*length(list(...))-sum(...),
               C4P248_1,C4P248_2,C4P248_3,C4P248_4,C4P248_5,
               C4P248_6,C4P248_7,C4P248_8,C4P248_9,C4P248_10,
               C4P248_11,C4P248_12)>0)
          )] %>%
  svydesign(ids =~ psu1+psu2,
            weights =~ peso,
            data = .)
) %>%
  evaluate_plan(
    rules = list(INEIDIR__ = ineidir),
    expand = FALSE
  )