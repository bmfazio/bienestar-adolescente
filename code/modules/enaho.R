### Cargar ENAHO

# ENAHO01A-2017-300.SAV - educacion
# ENAHO01A-2017-400.SAV - salud
# ENAHO-2017-606.SAV - esparcimiento diversion etc
# ENAHO-2017-800A.SAV/ENAHO-2017-800B.SAV - participacion ciudadana

## Estructura del ID: AÑO+CONGLOME+VIVIENDA+HOGAR+CODPERSO

# Modulo 200-Info basica de personas del hogar
enaho01.200 <- import("enaho/2017/603-Modulo02/Enaho01-2017-200.sav" %>% paste0(ineidir,.), setclass = "data.table")
enaho01.200[,id:=paste0(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO)] # crear codperso
# Modulo 300-Educacion
enaho01a.300<- import("enaho/2017/603-Modulo03/Enaho01A-2017-300.sav" %>% paste0(ineidir,.), setclass = "data.table")
enaho01a.300[,id:=paste0(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO)] # crear codperso
# Modulo 500-Trabajo
enaho01a.500<- import("enaho/2017/603-Modulo05/Enaho01A-2017-500.sav" %>% paste0(ineidir,.), setclass = "data.table")
enaho01a.500[,id:=paste0(AÑO, CONGLOME, VIVIENDA, HOGAR, CODPERSO)] # crear codperso

# Info de hogar para linea de pobreza
#enhano <- import("/home/bmfazio/Documents/datasets/inei/enaho/2017/603-Modulo34/Sumaria-2017.sav", setclass = "data.table")
#ENAHO

enaho01.200[,.(id, psu=CONGLOME, stratum=ESTRATO, weight=FACPOB07,
               edad=P208A,
               sexo=putlabel(P207),
               trab200=as.vector(2-P210),
               trab200.real=as.numeric(!(P211A%in%8:9)),
               trab200.tiempo=P211D)] %>%
  merge(enaho01a.300[,.(id,
                        lenmaterna=P300A,
                        leer.auto=P302,
                        leer.cart=P302X,
                        educ.aprobado=P301A,
                        educ.ultanho=P304A,
                        educ.esteanho=P308A,
                        educ.tvet=as.numeric(P313==8&!is.na(P313)),
                        estudia.actual=as.vector(2-P306),
                        internet.ultmes=(2-P314A)%+rmna%0
                        )], by="id", all.x = TRUE) %>%
  merge(enaho01a.500[,.(id,
                        trab500=((2-P501)%+rmna%(2-P502)%+rmna%(2-P503)%+rmna%(2-P5041)%+rmna%(2-P5042)%+rmna%
                                   (2-P5043)%+rmna%(2-P5044)%+rmna%(2-P5045)%+rmna%(2-P5046)%+rmna%(2-P5047)%+rmna%
                                   (2-P5048)%+rmna%(2-P5049)%+rmna%(2-P50410)%+rmna%(2-P50411)),
                        trab500.tiempo=max(ifelse(is.na(P513T),0,P513T)+ifelse(is.na(P518),0,P518), P520, na.rm=T)
                        )], by="id", all.x = TRUE) %>%
  svydesign(id=~psu, strat=~stratum, weight=~weight, data=.) -> denaho

# Porcentaje de adolescentes que usaron el internet en el último mes
denaho %>% subset(10<=edad&edad<=19) %>% svyciprop(~internet.ultmes, .)
# Tasa de finalización de educación primaria y tasa de finalización de secundaria
denaho %>% subset(14<=edad&edad<=16) %>% svyciprop(~I(educ.aprobado >= 4 & educ.aprobado != 12), .)
  #***que grupo para secundaria?
# Tasa de adolescentes fuera del Sistema educativo (out-of-school rate)
denaho %>% subset(10<=edad&edad<=17) %>% svyciprop(~estudia.actual, .)
# Tasa de matrícula bruta en educación superior (incluye los que ya completaron)
denaho %>% subset(18<=edad&edad<=22) %>% svyciprop(~I((educ.aprobado %in% c(8,10,11))|(educ.esteanho %in% 4:6)), .)
# Porcentaje de adolescentes involucrados en trabajo infantil
cat("AGREGA CUESTIONARIO 500")
denaho %>% subset(10<=edad&edad<=11) %>% svyciprop(~I((trab200==1|trab200.real==1)&trab200.tiempo>=1), .)
denaho %>% subset(12<=edad&edad<=13) %>% svyciprop(~I((trab200==1|trab200.real==1)&trab200.tiempo>=24), .)
denaho %>% subset(14<=edad&edad<=17) %>% svyciprop(~I((trab200==1|trab200.real==1)&trab200.tiempo>=36), .)
denaho %>% subset(10<=edad&edad<=17) %>%
  svyciprop(~I(
    (10<=edad&edad<=11&(trab200==1|trab200.real==1)&trab200.tiempo>=1)|
    (12<=edad&edad<=13&(trab200==1|trab200.real==1)&trab200.tiempo>=24)|
    (14<=edad&edad<=17&(trab200==1|trab200.real==1)&trab200.tiempo>=36)), .)
# % of 15-19 year-olds enrolled in TVET*
  #313A. ¿CUÁL ES LA PRINCIPAL RAZÓN POR LA QUE NO ESTÁ MATRICULADO O NO ASISTE A ALGÚN CENTRO O PROGRAMA DE EDUCACIÓN BÁSICA O SUPERIOR?
  # Asiste a un centro de Educación Técnico Productiva ....... 8
denaho %>% subset(15<=edad&edad<=19) %>% svyciprop(~educ.tvet, .)
# Proporción de adolescentes entre 15-19 por debajo de la línea internacional de la pobreza
  # El indicador mide el número de adolescentes que pertenecen a hogares con un nivel de gasto inferior a la línea internacional de pobreza.
  # Hay dos fuentes de gasto: Sumaria por 8 y por 12 grupos de gasto
  # Hay dos items sobre gasto: Gasto total monetario, Gasto total bruto.
  # (por que gasto y no ingreso?)
  # Variables:
    # Linea de pobreza total, Linea de pobreza alimentaria
    # POBREZA: Pobre extremo, no extremo, no pobre <- usar esto?
cat("no olvides la linea de pobreza")

# Tasa de desempleo adolescente
  # El tema de busqueda esta un poco complicado
# Porcentaje de adolescentes sin educación, empleo o formación (NEET)
#denaho %>% subset(10<=edad&edad<=17) %>% svyciprop(~I(!estudia.actual&!trabajando&!trabajo.real), .)

### < achori >
# Pobreza multidimensional
# definicion: Proporción de adolescentes en pobreza multi-dimensional http://hdr.undp.org/en/2018-MPI
enhano <- import("/home/bmfazio/Documents/datasets/inei/enaho/2017/603-Modulo01/Enaho01-2017-100.sav", setclass = "data.table")

### Health: no hay info de salud del adolescente
### Education: solo tomamos info del adolescente mismo?
### STANDARD OF LIVING:
#Cooking Fuel 	The household cooks with dung, wood, charcoal or coal.
cat("revisar cooking")
enhano$P1135==1|enhano$P1136==1|enhano$P1137==1 # "otro" considerar como "malo"?
#enhano$P1138 # "no cocinan" considerar como "malo"?
#Sanitation 	The household’s sanitation facility is not improved (according to SDG guidelines) or it is improved but shared with other households.
cat("pendiente improved facility")
  # cual es al definicion de SDG improved facility?
#Drinking Water 	The household does not have access to improved drinking water (according to SDG guidelines) or safe drinking water is at least a 30-minute walk from home, round trip.
  # No hay disponibilidad de informacion sobre distancia de la fuente o fuentes secundarias si fuente primaria no es fiable
cat("revisa drinking water")
(enhano$P110 %in% 1:3)&(enhano$P110A %in% 2)|(enhano$P110 %in% 4:6)
#Electricity 	The household has no electricity.
enhano$P1121==1
#Housing 	Housing materials for at least one of roof, walls and floor are inadequate: the floor is of natural materials and/or the roof and/or walls are of natural or rudimentary materials.
(enhano$P101 %in% c(5:7))|(enhano$P102 %in% 3:8)|(enhano$P103 %in% c(4,6))|(enhano$P103 %in% c(2,5:7))
#Assets 	The household does not own more than one of these assets: radio, TV, telephone, computer, animal cart, bicycle, motorbike or refrigerator, and does not own a car or truck.
