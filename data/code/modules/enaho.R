library(rio);library(data.table);library(dplyr);library(survey)
### Cargar ENAHO

# Registro de personas por hogar
enaho01.200 <- import("../source/enaho/2017/603-Modulo02/Enaho01-2017-200.sav", setclass = "data.table")
# Modulo Educacion
enaho01a.300<- import("../source/enaho/2017/603-Modulo03/Enaho01A-2017-300.sav", setclass = "data.table")
  # crear codperso
enaho01a.300[,P201P:=paste0(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO)]

#ENAHO
# Prevalencia de desnutricion o sobrepeso
# Prevalencia de la anemia
# Tasa de finalización de educación primaria y tasa de finalización de secundaria
# Tasa de adolescentes fuera del Sistema educativo (out-of-school rate)
# Tasa de matrícula bruta en educación superior
# Proporción de adolescentes en pobreza multi-dimensional
# Porcentaje de adolescentes involucrados en trabajo infantil
# % of 15-19 year-olds enrolled in TVET*
# Porcentaje de adolescentes sin educación, empleo o formación (NEET)
# Tasa de desempleo adolescente
# Proporción de adolescentes entre 15-19 por debajo de la línea internacional de la pobreza
# Porcentaje de adolescentes que usaron el internet en el último mes

## Estructura del ID (P201P): AÑO+CONGLOME+VIVIENDA+HOGAR+CODPERSO

# Tasa de finalización de educación primaria y tasa de finalización de secundaria
enaho01.200[,.(id=P201P, psu=)]


enaho01.200$FACPOB07 # pesitoh
enaho01a.300$FACTOR07 # pesitoh "expansion anual proyeciones censo 2007"
enaho01a.300$FACTOR07A # pesitoh "ajustado por grupos de edad" ???

enaho01.200$P208A # edad

enaho01.200$P210 # TRABAJANDO EN O FUERA DE HOGAR PARA INGRESO - SOLO <= 17 anh

# ENAHO01A-2017-300.SAV - educacion
# ENAHO01A-2017-400.SAV - salud
# ENAHO-2017-606.SAV - esparcimiento diversion etc
# ENAHO-2017-800A.SAV/ENAHO-2017-800B.SAV - participacion ciudadana

#  tan rarrros los numeros conglom
enaho01.200$CONGLOME %>% as.factor %>% as.numeric
enaho01.200$NCONGLOME %>% as.factor %>% as.numeric
enaho01.200[,.(length(unique(CONGLOME))),NCONGLOME]