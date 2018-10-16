## Registro Denuncias
library(rio);library(data.table);library(magrittr);library(survey)
# recuerda: edad, sexo, ubicacion victima
cap300 <- import("../source/reg-nac-denuncias-faltas/611-Modulo1313/Capítulo_300_Denuncia_de_Delitos_2017.sav", setclass = "data.table")

# Tasa de homicidio doloso
homicidio <- c(1, 3, 4, 6, 7, 14)
dprote.homicidio <- cap300[is.element(IH208, homicidio) & IVH305 >= 10 & IVH305 <= 19,
                           .(lugar=NOMBREDD, sexo=IVH303, edad=IVH305)]


# Tasa de denuncia de violencia doméstica
feminicidio <- 7
tentativa <- 25
lesiones <- c(15, 27, 28, 29, 30)
dprote.violenciad <- cap300[is.element(IH208, c(feminicidio, tentativa, lesiones)) & IVH305 >= 10 & IVH305 <= 19
                            & is.element(IVH310,c(1,3)),
                            .(lugar=NOMBREDD, sexo=IVH303, edad=IVH305)]

dprote.todopareja <- cap300[IVH305 >= 10 & IVH305 <= 19 & is.element(IVH310,c(1,3)),
                            .(lugar=NOMBREDD, sexo=IVH303, edad=IVH305)]