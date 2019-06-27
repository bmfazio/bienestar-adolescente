# ID de hogar       hh = AÑO+CONGLOME+VIVIENDA+HOGAR
# ID para persona   id = AÑO+CONGLOME+VIVIENDA+HOGAR+CODPERSO
enaho_load <- drake_plan(
  # Modulo 200-Info basica de personas en el hogar
  enaho01.200_2013 =
    enaho01.200_import(file_in("INEIDIR__/enaho/2013/404-Modulo02/Enaho01-2013-200.sav"), 2013),
  enaho01.200_2014 =
    enaho01.200_import(file_in("INEIDIR__/enaho/2014/440-Modulo02/Enaho01-2014-200.sav"), 2014),
  enaho01.200_2015 =
    enaho01.200_import(file_in("INEIDIR__/enaho/2015/498-Modulo02/Enaho01-2015-200.sav"), 2015),
  enaho01.200_2016 =
    enaho01.200_import(file_in("INEIDIR__/enaho/2016/546-Modulo02/Enaho01-2016-200.sav"), 2016),
  enaho01.200_2017 =
    enaho01.200_import(file_in("INEIDIR__/enaho/2017/603-Modulo02/Enaho01-2017-200.sav"), 2017),
  enaho01.200 =
    rbind(enaho01.200_2013, enaho01.200_2014, enaho01.200_2015, enaho01.200_2016, enaho01.200_2017),
  # Modulo 300-Educacion
  enaho01.300_2013 =
    enaho01.300_import(file_in("INEIDIR__/enaho/2013/404-Modulo03/Enaho01A-2013-300.sav")),
  enaho01.300_2014 =
    enaho01.300_import(file_in("INEIDIR__/enaho/2014/440-Modulo03/Enaho01A-2014-300.sav")),
  enaho01.300_2015 =
    enaho01.300_import(file_in("INEIDIR__/enaho/2015/498-Modulo03/Enaho01A-2015-300.sav")),
  enaho01.300_2016 =
    enaho01.300_import(file_in("INEIDIR__/enaho/2016/546-Modulo03/Enaho01A-2016-300.sav")),
  enaho01.300_2017 =
    enaho01.300_import(file_in("INEIDIR__/enaho/2017/603-Modulo03/Enaho01A-2017-300.sav")),
  enaho01a.300 =
    rbind(enaho01.300_2013, enaho01.300_2014, enaho01.300_2015, enaho01.300_2016, enaho01.300_2017),
  # Modulo 500-Trabajo
  enaho01.500_2013 =
    enaho01.500_import(file_in("INEIDIR__/enaho/2013/404-Modulo05/Enaho01A-2013-500.sav")),
  enaho01.500_2014 =
    enaho01.500_import(file_in("INEIDIR__/enaho/2014/440-Modulo05/Enaho01A-2014-500.sav")),
  enaho01.500_2015 =
    enaho01.500_import(file_in("INEIDIR__/enaho/2015/498-Modulo05/Enaho01A-2015-500.sav")),
  enaho01.500_2016 =
    enaho01.500_import(file_in("INEIDIR__/enaho/2016/546-Modulo05/Enaho01A-2016-500.sav")),
  enaho01.500_2017 =
    enaho01.500_import(file_in("INEIDIR__/enaho/2017/603-Modulo05/Enaho01A-2017-500.sav")),
  enaho01a.500 =
    rbind(enaho01.500_2013, enaho01.500_2014, enaho01.500_2015, enaho01.500_2016, enaho01.500_2017),
  # Sumaria - Cantidades calculadas por hogar (nivel de pobreza)
  enaho.sumaria_2013 =
    enaho.sumaria_import(file_in("INEIDIR__/enaho/2013/404-Modulo34/Sumaria-2013.sav")),
  enaho.sumaria_2014 =
    enaho.sumaria_import(file_in("INEIDIR__/enaho/2014/440-Modulo34/Sumaria-2014.sav")),
  enaho.sumaria_2015 =
    enaho.sumaria_import(file_in("INEIDIR__/enaho/2015/498-Modulo34/Sumaria-2015.sav")),
  enaho.sumaria_2016 =
    enaho.sumaria_import(file_in("INEIDIR__/enaho/2016/546-Modulo34/Sumaria-2016.sav")),
  enaho.sumaria_2017 =
    enaho.sumaria_import(file_in("INEIDIR__/enaho/2017/603-Modulo34/Sumaria-2017.sav")),
  enaho.sumaria =
    rbind(enaho.sumaria_2013, enaho.sumaria_2014, enaho.sumaria_2015, enaho.sumaria_2016, enaho.sumaria_2017)
) %>%
  evaluate_plan(
    rules = list(INEIDIR__ = ineidir),
    expand = FALSE
  )

enaho_merge <- drake_plan(
  enaho_ready =
    enaho01.200[,.(
      id, hh,
      region.25 = region,
      region.css = DOMINIO,
      psu = CONGLOME,
      stratum = ESTRATO,
      area = ifelse(ESTRATO %in% 7:8, "RURAL", "URBANA"),
      weight = FACPOB07,
      edad = P208A,
      sexo = putlabel(P207) %>% toupper,
      trab200 = as.numeric(((2-P210)%+rmna%!(P211A%in%8:9))==2),
      trab200.tiempo = P211D
      )] %>%
    merge(enaho01a.300[,.(
      id,
      lenmaterna = P300A,
      leer.auto = P302,
      leer.cart = P302X,
      educ.aprobado = P301A,
      educ.ultanho = P304A,
      educ.esteanho = P308A,
      educ.tvet = as.numeric(P313==8&!is.na(P313)), # (informacion limitada)
      estudia.actual = as.vector(2-P306),
      internet.ultmes = (2-P314A)%+rmna%0
      )],
      by="id", all.x = TRUE) %>%
    merge(enaho01a.500[,.(
        id,
        trab500 =
          (2-P501)%+rmna%
          (2-P502)%+rmna%
          (2-P503)%+rmna%
          (2-P5041)%+rmna%
          (2-P5042)%+rmna%
          (2-P5043)%+rmna%
          (2-P5044)%+rmna%
          (2-P5045)%+rmna%
          (2-P5046)%+rmna%
          (2-P5047)%+rmna%
          (2-P5048)%+rmna%
          (2-P5049)%+rmna%
          (2-P50410)%+rmna%
          (2-P50411),
        trab500.tiempo =
          mapply(max,
                 ifelse(is.na(P513T),0,P513T) +
                   ifelse(is.na(P518),0,P518),
                 ifelse(is.na(P520),0,P520),
                 na.rm=T),
        trab500.buscando = 2-P545,
        autoreporte.estudiando = as.numeric(P546==4&!is.na(P546))
        )],
        by="id", all.x = TRUE) %>%
    merge(enaho.sumaria[,.(
      hh,
      pobreza = POBREZA
      )],
      by="hh", all.x = TRUE) %>%
    mutate(
      region = case_when(
        region.25 == "15" & region.css == 8 ~ "LIMA PROVINCIA",
        TRUE ~ region.25 %>% ubigeator
        )
      ) %>%
    svydesign(ids =~ psu,
              strata =~ stratum,
              weights =~ weight,
              data = .)
)