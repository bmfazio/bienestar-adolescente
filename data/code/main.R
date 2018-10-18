#Bases que necesitare:
# [ ] SIAGIE
# [ ] SINADEF
# [ ] CDC
# [ ] CNV
# [ ] HIS
# 

# [mas reciente]
# [2015] ENARES
# [2015] ETI
# [2010] ENUT
# .[2017] ENAHO
# >[2017] ENDES
# >[2017] Registro Denuncias (poquisima data, dudo que sea util)
# [ ] GSHS
# [ ] PISA
# [ ] CENAN
# [ ] ECE

putlabel <- function(x) {
  if(is.null(attr(x,"labels"))){
    stop("No 'labels' attribute")
  } else {
    return(factor(x, levels = attr(x,"labels"), labels = names(attr(x,"labels"))))
  }
}

source("modules/endes.R")

library(googlesheets)
#gs_new("ENDES-tab", ws_title = "Indicadores ENDES 2017", input = tab.endes, trim = TRUE)