### Cargar ENAHO

# ENAHO01A-2017-300.SAV - educacion
# ENAHO01A-2017-400.SAV - salud
# ENAHO-2017-606.SAV - esparcimiento diversion etc
# ENAHO-2017-800A.SAV/ENAHO-2017-800B.SAV - participacion ciudadana

## Estructura del ID: AÑO+CONGLOME+VIVIENDA+HOGAR+CODPERSO

# Registro de personas por hogar
enaho01.200 <- import("enaho/2017/603-Modulo02/Enaho01-2017-200.sav" %>% paste0(ineidir,.), setclass = "data.table")
enaho01.200[,id:=paste0(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO)] # crear codperso
# Modulo Educacion
enaho01a.300<- import("enaho/2017/603-Modulo03/Enaho01A-2017-300.sav" %>% paste0(ineidir,.), setclass = "data.table")
enaho01a.300[,id:=paste0(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO)] # crear codperso

#ENAHO

# Porcentaje de adolescentes que usaron el internet en el último mes
#> Tasa de finalización de educación primaria y tasa de finalización de secundaria
  # que pregunta consideramos, que edades consideramos
#> Tasa de adolescentes fuera del Sistema educativo (out-of-school rate)
  # definicion exacta
#> Tasa de matrícula bruta en educación superior (incluye los que ya completaron)

enaho01.200[,.(id, psu=CONGLOME, stratum=ESTRATO, weight=FACPOB07,
               edad=P208A, sexo=putlabel(P207),
               trabajando=as.vector(2-P210), trabajo.real=as.numeric(!(P211A%in%8:9)), trabajo.tiempo=P211D)] %>%
  merge(enaho01a.300[,.(id,lenmaterna = P300A, leer.auto = P302, leer.cart = P302X,
                        educ.aprobado = P301A, educ.ultanho = P304A, educ.esteanho = P308A,
                        estudia.actual = as.vector(2-P306)
                        )], by="id") %>%
  svydesign(id=~psu, strat=~stratum, weight=~weight, data=.) -> denaho

# Tasa de finalización de educación primaria y tasa de finalización de secundaria
denaho %>% subset(14<=edad&edad<=16) %>% svyciprop(~I(educ.aprobado >= 4 & educ.aprobado != 12), .)
  #***que grupo para secundaria?
# Tasa de adolescentes fuera del Sistema educativo (out-of-school rate)
denaho %>% subset(10<=edad&edad<=17) %>% svyciprop(~estudia.actual, .)
# Tasa de matrícula bruta en educación superior (incluye los que ya completaron)
denaho %>% subset(18<=edad&edad<=22) %>% svyciprop(~I((educ.aprobado %in% c(8,10,11))|(educ.esteanho %in% 4:6)), .)
# Porcentaje de adolescentes involucrados en trabajo infantil
denaho %>% subset(10<=edad&edad<=11) %>% svyciprop(~I((trabajando==1|trabajo.real==1)&trabajo.tiempo>=1), .)
denaho %>% subset(12<=edad&edad<=13) %>% svyciprop(~I((trabajando==1|trabajo.real==1)&trabajo.tiempo>=24), .)
denaho %>% subset(14<=edad&edad<=17) %>% svyciprop(~I((trabajando==1|trabajo.real==1)&trabajo.tiempo>=36), .)
denaho %>% subset(10<=edad&edad<=17) %>%
  svyciprop(~I(
    (10<=edad&edad<=11&(trabajando==1|trabajo.real==1)&trabajo.tiempo>=1)|
    (12<=edad&edad<=13&(trabajando==1|trabajo.real==1)&trabajo.tiempo>=24)|
    (14<=edad&edad<=17&(trabajando==1|trabajo.real==1)&trabajo.tiempo>=36)), .)
# Porcentaje de adolescentes sin educación, empleo o formación (NEET)
denaho %>% subset(10<=edad&edad<=17) %>% svyciprop(~I(!estudia.actual&!trabajando&!trabajo.real), .)
# Tasa de desempleo adolescente VER Cuestionario 500 tambien tiene info de empleo para 14a+
# ??? % of 15-19 year-olds enrolled in TVET*
# ??? Proporción de adolescentes entre 15-19 por debajo de la línea internacional de la pobreza
# definicion: Proporción de adolescentes en pobreza multi-dimensional http://hdr.undp.org/en/2018-MPI
