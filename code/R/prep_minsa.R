minsa_load <- drake_plan(
  minsa.defun =
    (function(x){
      read.xlsx(x,
               sheetIndex = 1) -> tmp
      tmp[,2:4] <- apply(tmp[,2:4], 2,
                          iconv, from = "utf8", to = "ASCII//TRANSLIT")
      tmp[,2:4] <- apply(tmp[,2:4], 2,
                         function(x){
                           stri_replace_all_regex(x, "[-_]",
                                                  replacement = " ")
                         })
      
      colnames(tmp)[5] <- "EDAD"
      tmp %>%
        mutate(icd10 = SUBCATEGORIA %>%
                 sapply(function(x)stri_split_fixed(x, "-")[[1]][1])) %>%
        mutate(suicidio =
                 ifelse(
                   substr(icd10, 1, 3) %in%
                     paste0("X", c(60:84)),
                   1, 0),
               transito =
                 ifelse(
                   substr(icd10, 1, 3) %in%
                     paste0("V", sprintf("%02d", c(1:5, 10:15, 20:79))),
                   1, 0)) %>%
        group_by(UBIGEO.RH, DEPARTAMENTO.RH, PROVINCIA.RH,
                 DISTRITO.RH, SEXO, EDAD) %>%
        summarise(global = sum(CASOS),
                  suicidio = sum(suicidio),
                  transito = sum (transito)) -> tmp
      tmp[tmp$PROVINCIA.RH == "LIMA",]$DEPARTAMENTO.RH <- "LIMA METROPOLITANA"
      tmp[tmp$DEPARTAMENTO.RH == "LIMA",]$DEPARTAMENTO.RH <- "LIMA REGION"
      tmp[tmp$PROVINCIA.RH == "NAZCA",]$PROVINCIA.RH <- "NASCA"
      tmp[tmp$DISTRITO.RH == "NAZCA",]$DISTRITO.RH <- "NASCA"
      tmp$SEXO <- ifelse(tmp$SEXO == "FEMENINO", "MUJER", "HOMBRE")
      tmp
    })(file_in("DATADIR__/minsa/Defunciones_11a19.xlsx")),
  minsa.depre =
    (function(x){
      read.xlsx(x,
               sheetIndex = 2, startRow = 2) -> tmp
      tmp[,1:3] <- apply(tmp[,1:3], 2,
                          iconv, from = "utf8", to = "ASCII//TRANSLIT")
      tmp[,1:3] <- apply(tmp[,1:3], 2,
                         function(x){
                           stri_replace_all_regex(x, "[-_]",
                                                  replacement = " ")
                         })
      
      
      tmp[apply(tmp[,1:3], 2, function(x)stri_detect_fixed(x, "Total")) %>%
          apply(1, function(x) !as.logical(sum(x, na.rm = T))),] -> tmp
      
      tmp %>%
        fillNAs %>%
        select_at(c(1:3,5)) -> tmp
      
      colnames(tmp)[4] <- "casos"
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
    })(file_in("DATADIR__/minsa/CASOS DE DEPRESION 2017.xlsx"))
) %>%
  evaluate_plan(
    rules = list(DATADIR__ = datadir),
    expand = FALSE
)