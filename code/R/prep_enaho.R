# ID de hogar       hh = AÑO+CONGLOME+VIVIENDA+HOGAR
# ID para persona   id = AÑO+CONGLOME+VIVIENDA+HOGAR+CODPERSO
enaho_load <- drake_plan(
  # Modulo 200-Info basica de personas en el hogar
  enaho01.200 =
    import(
      file_in("INEIDIR__/enaho/2017/603-Modulo02/Enaho01-2017-200.sav")) %>%
    mutate(
      region = substr(UBIGEO, 1, 2),
      hh = paste0(`AÑO`, CONGLOME, VIVIENDA, HOGAR),
      id = paste0(`AÑO`, CONGLOME, VIVIENDA, HOGAR, CODPERSO)) %>%
    as.data.table,
  # Modulo 300-Educacion
  enaho01a.300 =
    import(
      file_in("INEIDIR__/enaho/2017/603-Modulo03/Enaho01A-2017-300.sav")) %>%
    mutate(
      hh = paste0(`AÑO`, CONGLOME, VIVIENDA, HOGAR),
      id = paste0(`AÑO`, CONGLOME, VIVIENDA, HOGAR, CODPERSO)) %>%
    as.data.table,
  # Modulo 500-Trabajo
  enaho01a.500 =
    import(
      file_in("INEIDIR__/enaho/2017/603-Modulo05/Enaho01A-2017-500.sav")) %>%
    mutate(
      hh = paste0(`AÑO`, CONGLOME, VIVIENDA, HOGAR),
      id = paste0(`AÑO`, CONGLOME, VIVIENDA, HOGAR, CODPERSO)) %>%
    as.data.table,
  # Sumaria - Cantidades calculadas por hogar (nivel de pobreza)
  enaho.sumaria =
    import(file_in("INEIDIR__/enaho/2017/603-Modulo34/Sumaria-2017.sav")) %>%
    mutate(
      hh = paste0(`AÑO`, CONGLOME, VIVIENDA, HOGAR)) %>%
    as.data.table
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
        region.25 == "15" & region.css == 8 ~ "LIMA METROPOLITANA",
        TRUE ~ region.25 %>% ubigeator
        )
      ) %>%
    svydesign(ids =~ psu,
              strata =~ stratum,
              weights =~ weight,
              data = .)
)