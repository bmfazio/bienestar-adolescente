library(rio);library(data.table);library(dplyr);library(survey)
### Cargar ENAHO

# Registro de personas por hogar
enaho01.200 <- import("../source/enaho/2017/603-Modulo02/Enaho01-2017-200.sav", setclass = "data.table")
# Modulo Educacion
enaho01a.300<- import("../source/enaho/2017/603-Modulo03/Enaho01A-2017-300.sav", setclass = "data.table")
  # crear codperso
enaho01a.300[,id:=paste0(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO)]

#ENAHO

# ??? definicion: Proporción de adolescentes en pobreza multi-dimensional
# Porcentaje de adolescentes involucrados en trabajo infantil
# ??? % of 15-19 year-olds enrolled in TVET*
# ??? Porcentaje de adolescentes sin educación, empleo o formación (NEET)
# ??? Tasa de desempleo adolescente
# ??? Proporción de adolescentes entre 15-19 por debajo de la línea internacional de la pobreza

# Porcentaje de adolescentes que usaron el internet en el último mes
#> Tasa de finalización de educación primaria y tasa de finalización de secundaria
# que pregunta consideramos, que edades consideramos
#> Tasa de adolescentes fuera del Sistema educativo (out-of-school rate)
# definicion exacta
#> Tasa de matrícula bruta en educación superior (incluye los que ya completaron)

## Estructura del ID (P201P): AÑO+CONGLOME+VIVIENDA+HOGAR+CODPERSO

enaho01.200[,.(id=P201P, psu=CONGLOME, stratum=ESTRATO, weight=FACPOB07,
               edad=P208A, trabajo=P210)] %>%
  merge(enaho01a.300[,.(id,lenmaterna = P300A, leer.auto = P302, leer.cart = P302X,
                        educ.aprobado = P301A, educ.ulta = P304A, educ.este = P308A)], by="id") %>%
  svydesign(id=~psu, strat=~stratum, weight=~weight, data=.) -> denaho

# Tasa de finalización de educación primaria y tasa de finalización de secundaria
denaho %>% subset(14<=edad&edad<=16) %>% svyciprop(~I(educ.aprobado >= 4 & educ.aprobado != 12), .)
  # que grupo para secundaria?
# Tasa de adolescentes fuera del Sistema educativo (out-of-school rate)
  # como definir esto?
# Tasa de matrícula bruta en educación superior (incluye los que ya completaron)
denaho %>% subset(18<=edad&edad<=22) %>% svyciprop(~I((educ.aprobado %in% c(8,10,11))|(educ.este %in% 4:6)), .)

# ENAHO01A-2017-300.SAV - educacion
# ENAHO01A-2017-400.SAV - salud
# ENAHO-2017-606.SAV - esparcimiento diversion etc
# ENAHO-2017-800A.SAV/ENAHO-2017-800B.SAV - participacion ciudadana
