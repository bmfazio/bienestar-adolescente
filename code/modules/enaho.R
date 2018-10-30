out.enaho <- function() {
  ## ID de hogar       hh = AÑO+CONGLOME+VIVIENDA+HOGAR
  ## ID para persona   id = AÑO+CONGLOME+VIVIENDA+HOGAR+CODPERSO
  # Modulo 200-Info basica de personas del hogar
  enaho01.200 <- import("enaho/2017/603-Modulo02/Enaho01-2017-200.sav" %>% paste0(ineidir,.), setclass = "data.table")
  enaho01.200[,hh:=paste0(AÑO, CONGLOME, VIVIENDA, HOGAR)]
  enaho01.200[,id:=paste0(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO)]
  # Modulo 300-Educacion
  enaho01a.300<- import("enaho/2017/603-Modulo03/Enaho01A-2017-300.sav" %>% paste0(ineidir,.), setclass = "data.table")
  enaho01a.300[,hh:=paste0(AÑO, CONGLOME, VIVIENDA, HOGAR)]
  enaho01a.300[,id:=paste0(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO)]
  # Modulo 500-Trabajo
  enaho01a.500<- import("enaho/2017/603-Modulo05/Enaho01A-2017-500.sav" %>% paste0(ineidir,.), setclass = "data.table")
  enaho01a.500[,hh:=paste0(AÑO, CONGLOME, VIVIENDA, HOGAR)]
  enaho01a.500[,id:=paste0(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO)]
  # Medidas de resumen a nivel de hogar (incluye indicador de pobreza monetaria)
  enaho.sumaria <- import("/home/bmfazio/Documents/datasets/inei/enaho/2017/603-Modulo34/Sumaria-2017.sav", setclass = "data.table")
  enaho.sumaria[,hh:=paste0(AÑO, CONGLOME, VIVIENDA, HOGAR)]
  
  #ENAHO
  enaho01.200[,.(id, hh,
                 psu=CONGLOME, stratum=ESTRATO, weight=FACPOB07,
                 edad=P208A,
                 sexo=putlabel(P207),
                 trab200=as.numeric(((2-P210)%+rmna%!(P211A%in%8:9))==2),
                 trab200.tiempo=P211D)] %>%
    merge(enaho01a.300[,.(id,
                          lenmaterna=P300A,
                          leer.auto=P302,
                          leer.cart=P302X,
                          educ.aprobado=P301A,
                          educ.ultanho=P304A,
                          educ.esteanho=P308A,
                          educ.tvet=as.numeric(P313==8&!is.na(P313)), # muy poca info
                          estudia.actual=as.vector(2-P306),
                          internet.ultmes=(2-P314A)%+rmna%0
                          )], by="id", all.x = TRUE) %>%
    merge(enaho01a.500[,.(id,
                          trab500=((2-P501)%+rmna%(2-P502)%+rmna%(2-P503)%+rmna%(2-P5041)%+rmna%(2-P5042)%+rmna%
                                     (2-P5043)%+rmna%(2-P5044)%+rmna%(2-P5045)%+rmna%(2-P5046)%+rmna%(2-P5047)%+rmna%
                                     (2-P5048)%+rmna%(2-P5049)%+rmna%(2-P50410)%+rmna%(2-P50411)),
                          trab500.tiempo=
                            mapply(max,ifelse(is.na(P513T),0,P513T)+ifelse(is.na(P518),0,P518),ifelse(is.na(P520),0,P520),na.rm=T),
                          trab500.buscando=2-P545,
                          autoreporte.estudiando=as.numeric(P546==4&!is.na(P546))
                          )], by="id", all.x = TRUE) %>%
    merge(enaho.sumaria[,.(hh,
                           pobreza=POBREZA)
                        ], by="hh", all.x = TRUE) %>%
    svydesign(ids=~psu, strata=~stratum, weights=~weight, data=.) -> denaho
  
  list(
    # Porcentaje de adolescentes que usaron el internet en el último mes
    denaho %>% subset(12<=edad&edad<=17) %>% svyciprop(~internet.ultmes, .),
    # Tasa de finalización de educación primaria y tasa de finalización de secundaria
      # definicion a edad "por ley"
    denaho %>% subset(12<=edad&edad<=13) %>% svyciprop(~I(educ.aprobado >= 4 & educ.aprobado != 12), .),
    denaho %>% subset(17<=edad&edad<=18) %>% svyciprop(~I(educ.aprobado >= 6 & educ.aprobado != 12), .),
      #***que grupo para secundaria?
    # Tasa de adolescentes fuera del Sistema educativo (out-of-school rate)
    denaho %>% subset(12<=edad&edad<=17) %>% svyciprop(~!estudia.actual, .),
    # Tasa de matrícula bruta en educación superior (incluye los que ya completaron)
    denaho %>% subset(18<=edad&edad<=22) %>% svyciprop(~I((educ.aprobado %in% c(8,10,11))|(educ.esteanho %in% 4:6)), .),
    # Porcentaje de adolescentes involucrados en trabajo infantil
    denaho %>% subset(12<=edad&edad<=13) %>% svyciprop(~I((trab500.tiempo%+rmna%ifelse(trab200,trab200.tiempo,0))>=24), .),
    denaho %>% subset(14<=edad&edad<=17) %>% svyciprop(~I((trab500.tiempo%+rmna%ifelse(trab200,trab200.tiempo,0))>=36), .),
    denaho %>% subset(12<=edad&edad<=17) %>%
      svyciprop(~I(
        (12<=edad&edad<=13&((trab500.tiempo%+rmna%ifelse(trab200,trab200.tiempo,0))>=24))|
        (14<=edad&edad<=17&((trab500.tiempo%+rmna%ifelse(trab200,trab200.tiempo,0))>=36))), .),
    # Tasa de desempleo adolescente (denominador: buscando empleo + ya empleados)
    denaho %>% subset(15<=edad&edad<=19&((trab500.buscando&!trab500)|trab500)) %>% svyciprop(~I(!trab500), .),
    # Porcentaje de adolescentes sin educación, empleo o formación (NEET)
    denaho %>% subset(15<=edad&edad<=19) %>% svyciprop(~I(!estudia.actual&!trab500&!trab500.buscando), .),
    # Pobrezas
    denaho %>% subset(12<=edad&edad<=17) %>% svyciprop(~I(pobreza < 3), .)
    ) %>% lapply(svy2pci) %>% do.call(rbind, .) -> tmp.estimates
  
  colnames(tmp.estimates) <- c("valor", "CI95.Inf", "CI95.Sup")
  
  indnom <- c(
    "% 12-17 que usaron el internet en el último mes",
    "% 12-13 finalizo educación primaria",
    "% 17-18 finalizo educación secundaria",
    "% 12-17 fuera del Sistema educativo (out-of-school rate)",
    "% 18-22 matriculados en educación superior (graduados en numerador)",
    "% 12-13 en trabajo infantil",
    "% 14-17 en trabajo infantil",
    "% 12-17 en trabajo infantil",
    "% 15-19 desempleados",
    "% 15-19 nini",
    "% 12-17 en pobreza"
  )
  
  "OK" -> comments
  
  cbind(indnom,
        data.frame(round(tmp.estimates*c(rep(100,nrow(tmp.estimates))),2)),
        fuente = "ENAHO 2017") %>% list(content=., comments=comments)
}

out.enaho <- out.enaho()