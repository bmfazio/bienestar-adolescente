endes_indicators <- drake_plan(
  # ENDES MUJER
  tasa_natalidad =
    endes_mujer %>%
    subset(gedad == "15-19") %>%
    svy_mean(~ region+area, ~ nhijos) %>% add_mujer,
  prevalencia_mala.nutricion =
    endes_mujer %>%
    subset(gedad == "15-19") %>%
    svy_prop(~ region+area, ~ I(imc<1850|2500<imc)),
  prevalencia_anemia =
    endes_mujer %>%
    subset(gedad == "15-19") %>%
    svy_prop(~ region+area, ~ anemia),
  prevalencia_condon =
    endes_mujer %>%
    subset(gedad == "15-19" & inisex > 0) %>%
    svy_prop(~ region+area, ~ultsex.condon),
  prevalencia_metodo.moderno =
    endes_mujer %>%
    subset(gedad == "15-19" & inisex > 0) %>%
    svy_prop(~ region+area,  ~antic.moderno),
  matrimonio_infantil =
    endes_mujer %>%
    subset(gedad == "20-24") %>%
    svy_prop(~ region+area, ~I(edad.matri < 18)) %>% add_mujer,
  mujer_violentada =
    endes_mujer %>%
    subset(gedad == "15-19") %>%
    svy_prop(~ region+area, ~I(v.emoc==1|v.sex==1|v.fisi==1|v.fisigrav==1)),
  # ENDES SALUD
  prevalencia_tab.alc30d =
    endes_salud %>%
    subset(15<=edad&edad<=19) %>%
    svy_prop(~ region+sexo+area, ~(tabaco.30d+alc.30d) > 0)
)

enaho_indicators <- drake_plan(
  finalizacion_primaria =
    enaho_ready %>%
    subset(12<=edad&edad<=13) %>%
    svy_prop(~region+sexo+area, ~I(educ.aprobado >= 4 & educ.aprobado != 12)),
  finalizacion_secundaria =
    enaho_ready %>%
    subset(17<=edad&edad<=18) %>%
    svy_prop(~region+sexo+area, ~I(educ.aprobado >= 6 & educ.aprobado != 12)),
  out_of_school =
    enaho_ready %>%
    subset(12<=edad&edad<=17) %>%
    svy_prop(~region+sexo+area, ~!estudia.actual),
  matricula_superior =
  enaho_ready %>%
    subset(18<=edad&edad<=22) %>%
    svy_prop(~region+sexo+area,
             ~I((educ.aprobado %in% c(8,10,11))|(educ.esteanho %in% 4:6))),
  pobreza_monetaria =
    enaho_ready %>%
    subset(15<=edad&edad<=19) %>%
    svy_prop(~region+sexo+area, ~I(pobreza < 3)),
  trabajo_infantil_enaho =
    enaho_ready %>%
    subset(12<=edad&edad<=17) %>%
    svy_prop(~region+sexo+area, ~I(
      (12<=edad&edad<=14 & ((trab500.tiempo%+rmna%ifelse(trab200,trab200.tiempo,0))>=24))|
        (15<=edad&edad<=17 & ((trab500.tiempo%+rmna%ifelse(trab200,trab200.tiempo,0))>=36))
      )),
  desempleo_adolescente =
    enaho_ready %>%
    subset(15<=edad&edad<=19&((trab500.buscando&!trab500)|trab500)) %>%
    svy_prop(~region+sexo+area, ~I(!trab500)),
  porcentaje_nini =
    enaho_ready %>%
    subset(15<=edad&edad<=19) %>%
    svy_prop(~region+sexo+area, ~I(!estudia.actual&!trab500&!trab500.buscando)),
  uso_internet =
    enaho_ready %>%
    subset(12<=edad&edad<=17) %>%
    svy_prop(~region+sexo+area, ~internet.ultmes)
)

ece_indicators <- drake_plan(
  competencia_mate =
    data.frame(desag = ece_ready$desag,
               ind = ece_ready$math,
               se = 0),
  competencia_lect =
    data.frame(desag = ece_ready$desag,
               ind = ece_ready$read,
               se = 0)
)

enares_indicators <- drake_plan(
  violencia_cualquier =
    enares_ready %>%
    svy_prop(~region+sexo+area,
             ~I((casa.v.emo+casa.v.fis+cole.v.emo+cole.v.fis)>0)),
  violencia_no.bulli =
    enares_ready %>%
    svy_prop(~region+sexo+area,
             ~I((casa.v.emo+casa.v.fis)>0)),
  violencia_bullying =
    enares_ready %>%
    svy_prop(~region+sexo+area,
             ~I((cole.v.emo+cole.v.fis)>0)),
  violencia_sexual =
     enares_ready %>%
    svy_prop(~region+sexo+area, ~v.sex)
)

eti_indicators <- drake_plan(
  horas_trabajo.hogar =
    eti_ready %>%
    subset(10<=edad&edad<=17&!(is.na(tdomesticoLV)|is.na(tdomesticoSD))) %>%
    svy_mean(~region,
             ~I(tdomesticoLV.tiempo+tdomesticoSD.tiempo)),
  tinfantil_full =
    eti_ready %>% subset(12<=edad&edad<=17) %>%
    svy_prop(~region, ~I(
      (12<=edad&edad<=13&(
        trabtiempo+tdomesticoLV.tiempo+tdomesticoSD.tiempo)>=24)|
        (14<=edad&edad<=17&(
          trabtiempo+tdomesticoLV.tiempo+tdomesticoSD.tiempo)>=36))),
  tinfantil_shogar =
    eti_ready %>%
    subset(12<=edad&edad<=17) %>%
    svy_prop(~region, ~I(
      (12<=edad&edad<=13&(trabtiempo)>=24)|
        (14<=edad&edad<=17&(trabtiempo)>=36)))
)

enut_indicators <- drake_plan(
  participacion_sindicato =
    enut_ready %>%
    subset(12<=edad&edad<=17) %>%
    svy_prop(~sexo+area, ~sindicato),
  tiempo_recreativo =
    enut_ready %>%
    subset(12<=edad&edad<=17) %>%
    svy_prop(~sexo+area, ~I(tiempo.libre >= 24)),
  voluntariado =
    enut_ready %>%
    subset(12<=edad&edad<=17) %>%
    svy_prop(~sexo+area, ~voluntario)
)

pisa_indicators <- drake_plan(
  conoce_financiero = pisa_flt(pisa.finlit),
  aspira_superior = pisa_asp(pisa.qqq),
  vida_satisfac = pisa_sat(pisa.satisf)
)

cem_indicators <- drake_plan(
  cem_denuncias =
    (function(){
      tmp <- as.data.table(cem17)
      rbind(
        tmp[,.(desag = "NACIONAL", ind = sum(domestica, na.rm = T), se = 0)],
        tmp[,.(ind = sum(domestica, na.rm = T), se = 0), .(desag = region)],
        tmp[,.(ind = sum(domestica, na.rm = T), se = 0), .(desag = sex)],
        tmp[,.(ind = sum(domestica, na.rm = T), se = 0), .(desag = paste(region, sex, sep = "_"))]
      ) %>%
        merge(censo_desag_12a17, by = "desag", all.x = T) %>%
        transmute(desag, ind = ind*1000/pob, se)
        })()
)

iccs_indicators <- drake_plan(
  confianza_gob = 
    iccs %>%
    svy_prop(~sexo,
             ~I((confianza_gob)<=2)),
  opinion_cole =
    iccs %>%
    svy_prop(~sexo,
             ~I((opinion_cole)==4))
)

minsa_indicators <- drake_plan(
  minsa_depresion =
    (function(){
      tmp <- as.data.table(minsa.depre)
        rbind(
          tmp[,.(desag = "NACIONAL", ind = sum(casos), se = 0)],
          tmp[,.(ind = sum(casos), se = 0), .(desag = DEPARTAMENTO)],
          tmp[,.(ind = sum(casos), se = 0), .(desag = sexo)],
          tmp[,.(ind = sum(casos), se = 0), .(desag = paste(DEPARTAMENTO, sexo, sep = "_"))],
          tmp[,.(ind = sum(casos), se = 0), .(desag = paste(DEPARTAMENTO, PROVINCIA, DISTRITO, sep = "_"))],
tmp[,.(ind = sum(casos), se = 0), .(desag = paste(DEPARTAMENTO, PROVINCIA, DISTRITO, sexo, sep = "_"))]) %>%
          merge(censo_desag_12a17, by = "desag", all.x = T) %>%
          transmute(desag, ind = ind*1000/pob, se)
        })(),
  minsa_defunciones =
    (function(){
      tmp <- as.data.table(minsa.defun %>% filter(12<=EDAD&EDAD<=17))
        rbind(
          tmp[,.(desag = "NACIONAL", ind = sum(global), se = 0)],
          tmp[,.(ind = sum(global), se = 0), .(desag = DEPARTAMENTO.RH)],
          tmp[,.(ind = sum(global), se = 0), .(desag = SEXO)],
          tmp[,.(ind = sum(global), se = 0), .(desag = paste(DEPARTAMENTO.RH, PROVINCIA.RH, DISTRITO.RH, sep = "_"))],
          tmp[,.(ind = sum(global), se = 0), .(desag = paste(DEPARTAMENTO.RH, PROVINCIA.RH, DISTRITO.RH, SEXO, sep = "_"))]) %>%
          merge(censo_desag_12a17, by = "desag", all.x = T) %>%
          transmute(desag, ind = ind*1000/pob, se)
        })()    
)

plan_indicators <- bind_plans(
  endes_indicators,
  enaho_indicators,
  ece_indicators,
  enares_indicators,
  eti_indicators,
  enut_indicators,
  pisa_indicators,
  cem_indicators,
  iccs_indicators,
  minsa_indicators
)