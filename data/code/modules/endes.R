# ENDES
#   Tasa de fertilidad adolescente
#   Prevalencia de desnutricion o sobrepeso
#   Consumo de alcohol
#   Prevalencia de la anemia
#   Anticoncepción (necesidad satisfecha)
#   Uso de condon
#   Uso de Tabaco
#   Porcentaje de mujeres entre 20 y 24 años alguna vez unidas antes de los 15 años y antes de los 18 años
#   Porcentaje de adolescentes que han experimentado violencia física, sexual o emocional por parte de una pareja
#   Porcentaje de adolescentes que reportaron ser víctimas de bullying
#   Tasa de denuncia de violencia doméstica

library(rio);library(data.table);library(magrittr);library(survey)
### ENDES
## Respondent's basic data
rec0111 <- import("../source/endes/2017/Modulo66/REC0111.SAV", setclass = "data.table")
## Reproduction/Contraception
rec21 <- import("../source/endes/2017/Modulo67/REC21.SAV", setclass = "data.table")
re223132 <- import("../source/endes/2017/Modulo67/RE223132.SAV", setclass = "data.table")
re516171 <- import("../source/endes/2017/Modulo71/RE516171.SAV", setclass = "data.table")
re758081 <- import("../source/endes/2017/Modulo72/RE758081.SAV", setclass = "data.table")
## Salud (creo q solo para gestante/ninho)
#rec42 <- import("../source/endes/2017/Modulo70/REC42.SAV", setclass = "data.table")
## Cuestionario de Salud
csalud01 <- data.table(haven::read_sav("../source/endes/2017/Modulo414/CSALUD01.sav", encoding = "latin1"))
## Cuestionario de Salud (<12a)
csalud08 <- data.table(haven::read_sav("../source/endes/2017/Modulo414/CSALUD08.sav", encoding = "latin1"))
## Anemia mujer
rec44 <- import("../source/endes/2017/Modulo74/REC44.SAV", setclass = "data.table")
## Mortalidad materna
rec83 <- import("../source/endes/2017/Modulo73/REC83.SAV", setclass = "data.table")
## Violencia domestica
rec84dv <- import("../source/endes/2017/Modulo73/REC84DV.SAV", setclass = "data.table")

# Tasa de fertilidad adolescente
  # Pablo menciona natalidad en el documento:
  # La tasa de natalidad adolescente mide el número de nacimientos entre mujeres de 15-19 años por cada 1,000 mujeres en la misma cohorte.
dsalud.natalidad <- (rec0111[V013==1] %>% merge(re223132, by = "CASEID", all.x = T))[,.(peso=V005,
                                                                                        nhijos=V201)]

# Consumo de alcohol (15-19)
dsalud.alcohol <- csalud01[QS23>=15 & QS23<=19,.(peso=PESO15_AMAS, QHCLUSTER, QHNUMBER, QHHOME, QSNUMERO, QSSEXO,
                                                 alc.vida=2-QS206, alc.ano=2-QS208, alc.12v=2-QS209, alc.30d=2-QS210)]
dsalud.alcohol[alc.12v<0,alc.12v:=NA]

# Anticoncepción (necesidad satisfecha, cualquier metodo)
dsalud.anticoncept <- (rec0111[V013==1] %>%
                         merge(re223132, by = "CASEID", all.x = T) %>%
                         merge(re516171, by = "CASEID", all.x = T))[V525>0,.(peso=V005,
                                                                             antic.moderno=as.numeric(V364==1),
                                                                             antic.modotra=as.numeric(V364<=2))]

# Uso de condon
dsalud.condon <- (rec0111[V013==1] %>%
                    merge(re516171, by = "CASEID", all.x = T) %>%
                    merge(re758081, by = "CASEID", all.x = T))[V525>0,.(peso=V005,
                                                                        condon=V761)]

# Uso de Tabaco
dsalud.tabaco <- csalud01[QS23>=15 & QS23<=19,.(peso=PESO15_AMAS, QHCLUSTER, QHNUMBER, QHHOME, QSNUMERO, QSSEXO,
                                                tabaco.ano=2-QS200, tabaco.30d=2-QS201)]
dsalud.tabaco[tabaco.ano<0,tabaco.ano:=NA]
dsalud.tabaco[tabaco.30d<0,tabaco.30d:=NA]

# Porcentaje de adolescentes que han experimentado violencia física, sexual o emocional por parte de una pareja
dprote.pviolen <- (rec0111[V013==1] %>%
                     merge(rec84dv, by = "CASEID", all.x = T))[,.(peso=V005,
                                                                  v.emocional=D104, v.fisica=D106, v.fisicagrave=D107, v.sexual=D108)]
# pocas respuestas, tipo de violencia mas restringida pero incluye hombre
dprote.pviolen2 <- csalud01[QS23>=15 & QS23<=19,.(peso=PESO15_AMAS, QHCLUSTER, QHNUMBER, QHHOME, QSNUMERO, QSSEXO,
                                                  v.golpe=as.numeric(QS710>1), v.arma=as.numeric(QS711>1))]

# Porcentaje de adolescentes que reportaron ser víctimas de bullying
## error, Pablo quizo escribir ENARES

# Prevalencia de la anemia
rec44$HW57

# Porcentaje de mujeres entre 20 y 24 años alguna vez unidas antes de los 15 años y antes de los 18 años

# X primero ENAHO - Prevalencia de desnutricion o sobrepeso
# X primero RegDenuncias - Tasa de denuncia de violencia doméstica