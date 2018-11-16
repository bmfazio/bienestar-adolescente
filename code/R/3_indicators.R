endes_indicators <- drake_plan(
  tasa_natalidad =
    endes_mujer %>%
    subset(as.numeric(gedad) == 1) %>%
    svymean( ~ nhijos, .),
  prevalencia_mala.nutricion =
    endes_mujer %>%
    subset(as.numeric(gedad) == 1) %>%
    svyciprop( ~ I(imc<1850|2500<imc), .),
  prevalencia_anemia =
    endes_mujer %>%
    subset(as.numeric(gedad) == 1) %>%
    svyciprop( ~ anemia, .),
  prevalencia_alcohol30d =
    endes_salud %>%
    subset(15<=edad&edad<=19) %>%
    svyciprop(~alc.30d , .),
  prevalencia_tabaco30d =
    endes_salud %>%
    subset(15<=edad&edad<=19) %>%
    svyciprop(~tabaco.30d , .),
  prevalencia_condon =
    endes_mujer %>%
    subset(as.numeric(gedad) == 1 & inisex > 0) %>%
    svyciprop( ~ ultsex.condon, .),
  prevalencia_metodo.moderno =
    endes_mujer %>%
    subset(as.numeric(gedad) == 1 & inisex > 0) %>%
    svyciprop( ~ antic.moderno, .),
  matrimonio_infantil =
    endes_mujer %>%
    subset(as.numeric(gedad) == 2) %>%
    svyciprop( ~ I(edad.matri < 18), .),
  mujer_violentada =
    endes_mujer %>%
    subset(as.numeric(gedad) == 1) %>%
    svyciprop( ~ I(v.emoc==1|v.sex==1|v.fisi==1|v.fisigrav==1), .)
)

enaho_indicators <- drake_plan(
  finalizacion_primaria =
    enaho_ready %>%
    subset(12<=edad&edad<=13) %>%
    svyciprop(~I(educ.aprobado >= 4 & educ.aprobado != 12), .),
  finalizacion_secundaria =
    enaho_ready %>%
    subset(17<=edad&edad<=18) %>%
    svyciprop(~I(educ.aprobado >= 6 & educ.aprobado != 12), .),
  out_of_school =
    enaho_ready %>%
    subset(12<=edad&edad<=17) %>%
    svyciprop(~!estudia.actual, .),
  matricula_superior =
  enaho_ready %>%
    subset(18<=edad&edad<=22) %>%
    svyciprop(~I((educ.aprobado %in% c(8,10,11))|(educ.esteanho %in% 4:6)), .),
  pobreza_monetaria =
    enaho_ready %>%
    subset(15<=edad&edad<=19) %>%
    svyciprop(~I(pobreza < 3), .),
  trabajo_infantil_enaho =
    enaho_ready %>%
    subset(12<=edad&edad<=17) %>%
    svyciprop(~I(
      (12<=edad&edad<=13 & ((trab500.tiempo%+rmna%ifelse(trab200,trab200.tiempo,0))>=24))|
        (14<=edad&edad<=17 & ((trab500.tiempo%+rmna%ifelse(trab200,trab200.tiempo,0))>=36))
      ), .),
  desempleo_adolescente =
    enaho_ready %>%
    subset(15<=edad&edad<=19&((trab500.buscando&!trab500)|trab500)) %>%
    svyciprop(~I(!trab500), .),
  porcentaje_nini =
    enaho_ready %>%
    subset(15<=edad&edad<=19) %>%
    svyciprop(~I(!estudia.actual&!trab500&!trab500.buscando), .),
  uso_internet =
    enaho_ready %>%
    subset(12<=edad&edad<=17) %>%
    svyciprop(~internet.ultmes, .)
)

ece_indicators <- drake_plan(
  competencia_lect.mate =
    mean(ece_ready$Satisfactorio)
)

enares_indicators <- drake_plan(
  violencia_cualquier =
    enares_ready %>%
    svyciprop(~I((casa.v.emo+casa.v.fis+cole.v.emo+cole.v.fis)>0), .),
  violencia_bullying =
    enares_ready %>%
    svyciprop(~I((cole.v.emo+cole.v.fis)>0), .),
  violencia_sexual =
     enares_ready %>%
    svyciprop(~v.sex, .)
)

eti_indicators <- drake_plan(
  horas_trabajo.hogar =
    eti_ready %>%
    subset(10<=edad&edad<=17&!(is.na(tdomesticoLV)|is.na(tdomesticoSD))) %>%
    svymean( ~I(tdomesticoLV.tiempo+tdomesticoSD.tiempo), .),
  tinfantil_full =
    eti_ready %>% subset(12<=edad&edad<=17) %>%
    svyciprop(~I(
      (12<=edad&edad<=13&(trabtiempo+tdomesticoLV.tiempo+tdomesticoSD.tiempo)>=24)|
        (14<=edad&edad<=17&(trabtiempo+tdomesticoLV.tiempo+tdomesticoSD.tiempo)>=36)),.),
  tinfantil_shogar =
    eti_ready %>%
    subset(12<=edad&edad<=17) %>%
    svyciprop(~I(
      (12<=edad&edad<=13&(trabtiempo)>=24)|
        (14<=edad&edad<=17&(trabtiempo)>=36)), .)
)

enut_indicators <- drake_plan(
  enrolamiento_cetpro =
    enut_ready %>%
    subset(15<=edad&edad<=19) %>%
    svyciprop( ~ cetpro, .),
  participacion_sindicato =
    enut_ready %>%
    subset(12<=edad&edad<=17) %>%
    svyciprop( ~ sindicato, .),
  tiempo_recreativo =
    enut_ready %>%
    subset(12<=edad&edad<=17) %>%
    svymean( ~ tiempo.libre, .),    
  voluntariado =
    enut_ready %>%
    subset(12<=edad&edad<=17) %>%
    svyciprop( ~ voluntario, .)
)

plan_indicators <- bind_plans(
  endes_indicators,
  enaho_indicators,
  ece_indicators,
  enares_indicators,
  eti_indicators,
  enut_indicators
)