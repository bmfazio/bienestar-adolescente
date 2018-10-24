library(rio);library(data.table);library(dplyr);library(survey)

source("modules/config.R")

source("modules/endes.R")
source("modules/enaho.R")
source("modules/enares.R")
source("modules/eti.R")

# [NUMERO: anho mas reciente]

# .[2017] ENAHO
# >[2015] ENARES
# >[2017] ENDES
# >[2015] ETI
# >[2010] ENUT
  # % adolescentes que participan en actividades recreacionales/sociales por un periodo específico durante el día o la semana
  # Indicador de voluntariado
  # Participación en movimientos formales y no formales
# >X[2017] Registro Denuncias (poquisima data, dudo que sea util)

# Me falta conseguir (y son conseguibles, creo):
# [ ] GSHS
  # Consumo de alcohol
  # Uso de Tabaco
  # Prevalencia de depresion/ansiedad
  # Porcentaje de adolescentes que reportaron ser víctimas de bullying
  # Porcentaje de adolescentes que experimentan una conexión y regulación positiva
# [ ] PISA
  # Nivel de competencia lectura/matematicas
  # Habilidad TICs
  # Porcentaje de adolescentes con conocimientos básicos financieros y tenencia de ahorros
# [ ] CENAN
  # Prevalencia de desnutricion o sobrepeso
  # Prevalencia de la anemia
# [ ] ECE
  # Nivel de competencia lectura/matematicas

# Bases pendientes:
# [ ] SIAGIE
  # Tasa de adolescentes fuera del Sistema educativo (out-of-school rate)
  # % of 15-19 year-olds enrolled in TVET*
# [ ] SINADEF
# [ ] CDC
  # Muertes relacionadas al sida o nuevas infecciones por VIH
  # Tasa de mortalidad materna
# [ ] CNV
  # Tasa de fertilidad adolescente
# [ ] HIS
  # Prevalencia de depresion/ansiedad
# [ ] JNE
  # Porcentaje de adolescentes que tienen derecho a voto en las elecciones generales

#library(googlesheets)
#gs_new("ENDES-tab", ws_title = "Indicadores ENDES 2017", input = tab.endes, trim = TRUE)