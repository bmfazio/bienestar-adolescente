library(rio);library(data.table);library(dplyr);library(survey)

source("modules/config.R")
source("modules/endes.R")

# [NUMERO: anho mas reciente]
# [2015] ENARES
# [2015] ETI
# [2010] ENUT
# .[2017] ENAHO
# >[2017] ENDES
# >[2017] Registro Denuncias (poquisima data, dudo que sea util)

# Me falta conseguir (y son conseguibles, creo):
# [ ] GSHS
# [ ] PISA
# [ ] CENAN
# [ ] ECE

#Bases que necesitare:
# [ ] SIAGIE
# [ ] SINADEF
# [ ] CDC
# [ ] CNV
# [ ] HIS
# 

#library(googlesheets)
#gs_new("ENDES-tab", ws_title = "Indicadores ENDES 2017", input = tab.endes, trim = TRUE)