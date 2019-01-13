jne_load <- drake_plan(
  jne =
    (function(x){
      read.xlsx(x,
               sheetIndex = 1) -> tmp
      tmp[,1:3] <- apply(tmp[,1:3], 2,
                          iconv, from = "utf8", to = "ASCII//TRANSLIT")
      tmp[,1:3] <- apply(tmp[,1:3], 2,
                         function(x){
                           stri_replace_all_regex(x, "[-_]",
                                                  replacement = " ")
                         })
      
      tmp <- tmp %>% na.locf
      tmp <- as.data.table(tmp[-nrow(tmp),])
      names(tmp)[4] <- "casos"
      
      tmp[tmp$PROVINCIA == "LIMA",]$DEPARTAMENTO <- "LIMA METROPOLITANA"
      tmp[tmp$DEPARTAMENTO == "LIMA",]$DEPARTAMENTO <- "LIMA REGION"
      tmp[tmp$PROVINCIA == "ANTONIO RAIMONDI",]$PROVINCIA <- "ANTONIO RAYMONDI"
      tmp[tmp$PROVINCIA == "NAZCA",]$PROVINCIA <- "NASCA"
      tmp[tmp$DISTRITO == "NAZCA",]$DISTRITO <- "NASCA"
      tmp[tmp$DISTRITO == "SAN FRANCISCO DE YESO",]$DISTRITO <- "SAN FRANCISCO DEL YESO"
      tmp[tmp$DISTRITO == "MILPUCC",]$DISTRITO <- "MILPUC"
      tmp[tmp$DISTRITO == "HUAYLLO",]$DISTRITO <- "IHUAYLLO"
      tmp[tmp$DISTRITO == "HUAILLATI",]$DISTRITO <- "HUAYLLATI"
      tmp[tmp$DISTRITO == "MARISCAL GAMARRA",]$DISTRITO <- "GAMARRA"
      tmp[tmp$DISTRITO == "SANTA RITA DE SIHUAS",]$DISTRITO <- "SANTA RITA DE SIGUAS"
      tmp[tmp$DISTRITO == "ANDRES AVELINO CACERES D.",]$DISTRITO <- "ANDRES AVELINO CACERES DORREGARAY"
      tmp[tmp$DISTRITO == "QUISQUI",]$DISTRITO <- "QUISQUI (KICHKI)"
      tmp[tmp$DISTRITO == "SAN JUAN DE YSCOS",]$DISTRITO <- "SAN JUAN DE ISCOS"
      tmp[tmp$DISTRITO == "SAN JOSE DE LOS CHORRILLOS",]$DISTRITO <- "CUENCA"
      tmp[tmp$DISTRITO == "SAN FCO DE ASIS DE YARUSYACAN",]$DISTRITO <- "SAN FRANCISCO DE ASIS DE YARUSYACAN"
      tmp[tmp$DISTRITO == "CAPASO",]$DISTRITO <- "CAPAZO"
      tmp[tmp$DISTRITO == "RAIMONDI",]$DISTRITO <- "RAYMONDI"
      tmp[tmp$DISTRITO == "CORONEL GREGORIO ALBARRACIN L.",]$DISTRITO <- "CORONEL GREGORIO ALBARRACIN LANCHIPA"
      tmp
    })(file_in("DATADIR__/jne/ADX-2019-000418-2.xls"))
) %>%
  evaluate_plan(
    rules = list(DATADIR__ = datadir),
    expand = FALSE
)