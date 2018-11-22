## Registro Denuncias
# recuerda: edad, sexo, ubicacion victima
library(data.table)
library(rio)
library(dplyr)
setwd("D:/datasets/inei")
cap300 <- import("renadef/611-Modulo1313/Capítulo_300_Denuncia_de_Delitos_2017.sav",
                 setclass = "data.table")
cap200 <- import("renadef/611-Modulo1312/Capítulo_200_Denuncia_de_Delitos_2017.sav",
                 setclass = "data.table")
cap100 <- import("renadef/611-Modulo1311/Capítulo_100_Denuncia_de_Delitos_2017.sav",
                 setclass = "data.table")
# Tasa de homicidio doloso
homicidio <- c(1, 3, 4, 6, 7, 14)
               #  Homicidio simple Homicidio calificado (asesinato)   Homicidio por emoción violenta 
               #                 1                                3                                4 
               # Homicidio piadoso                      Feminicidio              Otros (especifique) 
               #                 6                                7                               14 
reg.homicidio <- cap300[is.element(IH208, homicidio) & IVH305 >= 10 & IVH305 <= 19,
                        .(lugar=NOMBREDD, sexo=IVH303, edad=IVH305)]

# Tasa de denuncia de violencia doméstica
feminicidio <- 7
tentativa <- 25
lesiones <- c(15, 27, 28, 29, 30)
# IVH310 en 1,3 = Esposo/Pareja/Conviviente
reg.violenciad <- cap300[is.element(IH208, c(feminicidio, tentativa, lesiones)) & IVH305 >= 10 & IVH305 <= 19
                         & is.element(IVH310,c(1,3)),
                         .(lugar=NOMBREDD, sexo=IVH303, edad=IVH305)]

reg.todopareja <- cap300[IVH305 >= 10 & IVH305 <= 19 & is.element(IVH310,c(1,3)),
                         .(lugar=NOMBREDD, sexo=IVH303, edad=IVH305)]


########
cap200$IH208_GENERICO %>% table
cap200[IH208_GENERICO == "DELITOS CONTRA LA HUMANIDAD",24:26]$IH208_ESPECIFICO %>% table
cap200[IH208_GENERICO == "DELITOS CONTRA LA LIBERTAD",24:26]$IH208_ESPECIFICO %>% table # incluye violacion y trafico de personas (libertad sexual/personal)
cap200[IH208_GENERICO == "DELITOS CONTRA LA FAMILIA",24:26]$IH208_ESPECIFICO %>% table # nada relevante
cap200[IH208_GENERICO == "DELITOS CONTRA LA VIDA, EL CUERPO Y LA SALUD",24:26]$IH208_ESPECIFICO %>% table

especificos <- c("VIOLACION DE LA LIBERTAD PERSONAL",
"VIOLACION DE LA LIBERTAD SEXUAL",
"VIOLACION DE LA INTIMIDAD",
"DESAPARICION FORZOSA",
"HECHOS SEGUIDOS DE MUERTE",
"HOMICIDIO",
"HOMICIDIO CULPOSO",
"INSTIGACION Y/O AYUDA AL SUICIDIO",
"LESIONES",
"TENTATIVA DE HOMICIDIO")

cap200[is.element(IH208_ESPECIFICO, especificos),24:26]$IH208_MODALIDAD %>% table

cap200[is.element(IH208_ESPECIFICO, especificos),24:26]$IH208_MODALIDAD %>% length

cap200[IH211_1==1|IH211_8==1|IH211_13==1|IH211_15==1|IH211_16==1|IH211_17==1|IH211_18==1] %>% dim



##############
library(stringi)
violcol <- apply(cap200, 2, function(x) grepl("violencia", tolower(x)))

whichvi <- apply(violcol, 2, sum)

cap200[violcol[,"IH208_MODALIDAD"],"IH208_MODALIDAD"]$IH208_MODALIDAD %>% table
