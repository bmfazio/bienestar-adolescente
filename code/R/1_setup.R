library(zoo)
library(rio)
library(yaml)
library(xlsx)
library(dplyr)
library(drake)
library(purrr)
library(tidyr)
library(intsvy)
library(readxl)
library(ggplot2)
library(survey)
library(stringi)
library(writexl)
library(data.table)

options(stringsAsFactors = FALSE)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

datadir <- read_yaml("config.yaml")[[paste(Sys.info(),collapse="|")]]

if(length(datadir) == 1 & any(dir.exists(datadir))){
  ineidir <- file.path(datadir, "inei")
  minedudir <- file.path(datadir, "minedu")
} else {
  stop("data dir not found")
}

harmean <- function(x, y)2/(x**-1 + y**-1)

indic2index <- function(x){
  x %>%
    group_by(region, provincia, distrito,
             area, sexo, dimension) %>%
    summarise(indice = mean(norm)) %>%
    bind_rows(
        mutate(., dimension = "GLOBAL") %>%
        group_by(region, provincia, distrito,
                 area, sexo, dimension) %>%
        summarise(indice =
                    exp(mean(log(
                      ifelse(indice < 0.01, 0.01, indice))))
                  ))
}

add_mujer <- function(x){
  x %>%
    mutate(desag =
             case_when(
               stri_count_fixed(desag, "_") == 0 &
                 !(desag %in% c("URBANA", "RURAL")) ~
                 paste0(desag, "_MUJER"),
               stri_detect_fixed(desag, "URBANA") ~
                 stri_replace_all_fixed(desag, "URBANA", "MUJER_URBANA"),
               stri_detect_fixed(desag, "RURAL") ~
                 stri_replace_all_fixed(desag, "RURAL", "MUJER_RURAL")
               )) %>% bind_rows(x, .)
}

avg_dims <- function(x, y) {
  y <- enquo(y)
  n <- (x %>% pull(!!y) %>% unique %>% setdiff(c("TOTAL", "MEDIA")) %>% length)
  x %>%
    filter(!!y != "TOTAL" & !!y != "MEDIA" ) %>%
    mutate(!!quo_name(y) := "MEDIA") %>%
    group_by(region, provincia, distrito, area, sexo,
             dimension, nombre, peor, mejor, fuente) %>%
    summarise(norm = sum(norm, n - length(norm))/n) %>%
    ungroup %>%
    bind_rows(x, .)
}

allind_func <- function(...) {
  list(...)[-1] %>%
    lapply(function(x){
      x %>%
        select(dimension, nombre) %>%
        unique
    }) -> inds
  indtypes <- names(inds)
  for(i in indtypes) {
    inds[[i]] %>%
      mutate(tipoind = 1) -> inds[[i]]
  }
  inds %>%
    reduce(left_join,
           by = c("nombre", "dimension")) -> inds
  names(inds)[stri_detect_fixed(names(inds),"tipoind")] <- indtypes
  inds %>%
    mutate_all(list(~case_when(
      is.na(.) ~ "",
      . == 1 ~ "X",
      TRUE ~ as.character(.))))
}

# LABELS
hm_ <- c("HOMBRE", "MUJER")
ur_ <- c("URBANA", "RURAL")

# Apply labels to categorical vars imported
# from other data formats
putlabel <- function(x) {
  if (is.null(attr(x, "labels"))) {
    stop("No 'labels' attribute")
  } else {
    return(as.character(factor(
      x,
      levels = attr(x, "labels"),
      labels = names(attr(x, "labels"))
    )))
  }
}

label_vals <- function(x, l = NULL){
  if(is.null(l))stop("Missing label type")
  if(l=="3regiones")return(
    case_when(x == 1 ~ "Lima metropolitana",
              x == 2 ~ "Resto Costa",
              x == 3 ~ "Sierra",
              x == 4 ~ "Selva")
  )
}

# Transform svy mean and proportion CI estimate objects
# into a simple 3-vector
svy2pci <- function(x) {
  xclass <- attr(x, "class")
  if(is.null(xclass)) {
    warning("not a svyciprop or svystat object, zero-width interval assumed")
    rep(x, 3)
  } else if (xclass == "svyciprop") {
    xci <- attr(x, "ci")
    c(as.vector(x), xci[1], xci[2])
  } else if (xclass == "svystat") {
    xsd <- sqrt(attr(x, "var"))
    z <- qnorm(0.975)
    as.vector(x) + c(0, -z*xsd, z*xsd)
  }
}

# Fixing issue with svyciprop
body(svyciprop)[[6]] <- substitute(names(rval) <- paste(deparse(formula[[2]]),collapse=""))

# Summing omitted questions
`%+rmna%` <- function(x, y) {
  x <- ifelse(is.na(x),0,x)
  y <- ifelse(is.na(y),0,y)
  x+y
}

# Easy paste
`%p0%` <- function(x, y) {
  paste0(x,y)
}


# Normalize indicators
normind <- function(x, max, min) {
  xn <- (x - min)/(max - min)
  case_when(
    xn > 1 ~ 1,
    xn < 0 ~ 0,
    TRUE ~ xn
  )
}

# Custom svy processing
svy_mean <- function(svyobj, desag, formulind){
  desag[[2]] %>%
  as.character %>%
  stri_replace_all(replacement = " ", fixed = "+") %>%
  paste(collapse=" ") %>%
  stri_split_fixed(pattern = " ") %>% unlist %>%
  subset(., nchar(.) > 0) -> desag
  as.data.frame(do.call(expand.grid, rep(list(0:1), length(desag)))[-1,]) -> scenarios
  
  for(i in 1:ncol(scenarios)){
    scenarios[, i] <- ifelse(scenarios[, i], desag[i], "")
  }
  scenarios %>%
    apply(1, function(x)paste(x[x!=""],collapse="+")) %>%
    paste("~",.) -> formuoli
  
  ssets <- list()
  for(i in 1:length(formuoli)){
    ssets[[i]] <- svyby(formulind, as.formula(formuoli[i]),
                        design = svyobj,
                        svymean, na.rm = T, vartype = "se")
  }
  lapply(ssets,
         function(x){
           n <- ncol(x)
           cbind(desag = apply(as.data.frame(x[, -c(ncol(x)-1,ncol(x))]), 1,
                               paste, collapse = "_"), x[, c(n-1,n)]) -> x
           colnames(x) <- c("desag", "ind", "se")
           x
           }) -> ssets
  
  global <- svyby(formulind, ~as.factor("NACIONAL"),
                  design = svyobj,
                  svymean, na.rm = T,
                  vartype = "se")
  colnames(global) <- c("desag", "ind", "se")
  ssets[[length(ssets)+1]] <- global
  bind_rows(ssets)
}

svy_prop <- function(svyobj, desag, formulind){
  
  svyobj <- svyobj %>% subset(!is.na(eval(formulind[[2]])))
  
  desag[[2]] %>%
  as.character %>%
  stri_replace_all(replacement = " ", fixed = "+") %>%
  paste(collapse=" ") %>%
  stri_split_fixed(pattern = " ") %>% unlist %>%
  subset(., nchar(.) > 0) -> desag
  as.data.frame(do.call(expand.grid, rep(list(0:1), length(desag)))[-1,]) -> scenarios
  
  for(i in 1:ncol(scenarios)){
    scenarios[, i] <- ifelse(scenarios[, i], desag[i], "")
  }
  scenarios %>%
    apply(1, function(x)paste(x[x!=""],collapse="+")) %>%
    paste("~",.) -> formuoli
  
  ssets <- list()
  for(i in 1:length(formuoli)){
    ssets[[i]] <- svyby(formulind, as.formula(formuoli[i]),
                        design = svyobj,
                        svyciprop, na.rm = T, vartype = "se")
  }
  lapply(ssets,
         function(x){
           n <- ncol(x)
           cbind(desag = apply(as.data.frame(x[, -c(ncol(x)-1,ncol(x))]), 1,
                               paste, collapse = "_"), x[, c(n-1,n)]) -> x
           colnames(x) <- c("desag", "ind", "se")
           x
           }) -> ssets
  
  global <- svyby(formulind, ~as.factor("NACIONAL"),
                  design = svyobj,
                  svyciprop, na.rm = T,
                  vartype = "se")
  colnames(global) <- c("desag", "ind", "se")
  ssets[[length(ssets)+1]] <- global
  bind_rows(ssets)
}

# Funciones de apoyo para tablas
tabfun <- function(source, name, fuente,
                   scale = 100,
                   highbad = T) {
  cbind(
    nombre = name,
    source %>%
      mutate(ind = ind*scale, error = 1.96*se*scale) %>%
      select(-se),
    fuente = fuente,
    highbad = highbad
  )
}

ubigeator <- function(x) {
  x <- trimws(x)
    case_when(
    x == "01" ~ "AMAZONAS",
    x == "02" ~ "ANCASH",
    x == "03" ~ "APURIMAC",
    x == "04" ~ "AREQUIPA",
    x == "05" ~ "AYACUCHO",
    x == "06" ~ "CAJAMARCA",
    x == "07" ~ "CALLAO",
    x == "08" ~ "CUSCO",
    x == "09" ~ "HUANCAVELICA",
    x == "10"~ "HUANUCO",
    x == "11"~ "ICA",
    x == "12"~ "JUNIN",
    x == "13"~ "LA LIBERTAD",
    x == "14"~ "LAMBAYEQUE",
    x %in% c("15","LIMA PROVINCIAS","LIMA") ~ "LIMA REGION",
    x == "16"~ "LORETO",
    x == "17"~ "MADRE DE DIOS",
    x == "18"~ "MOQUEGUA",
    x == "19"~ "PASCO",
    x == "20"~ "PIURA",
    x == "21"~ "PUNO",
    x == "22"~ "SAN MARTIN",
    x == "23"~ "TACNA",
    x == "24"~ "TUMBES",
    x == "25"~ "UCAYALI",
    TRUE ~ x
    )
}

# Excel export functions
xlsx.addTitle <- function(sheet, rowI, title, titleStyle, colI = 1){
  rows <-createRow(sheet, rowIndex=rowI)
  sheetTitle <- createCell(rows, colIndex=colI)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}
source("R/modules/export_all.R")

# ENDES
rech0_import <- function(path){
  import(path, setclass = "data.table") %>%
    setNames(., toupper(colnames(.))) %>%
    select(HHID, HV021, HV022, HV023, HV025)
}
rec0111_import <- function(path){
  import(path, setclass = "data.table") %>%
    setNames(., toupper(colnames(.))) %>%
    transmute(HHID = substr(CASEID, 1, 15), CASEID, V005, V013, V021, V022, V023, V025)
}
rech23_import <- function(path){
  import(path, setclass = "data.table") %>%
    setNames(., toupper(colnames(.))) %>%
    transmute(HHID, SHREGION)
}
re223132_import <- function(path){
  import(path, setclass = "data.table") %>%
    setNames(., toupper(colnames(.))) %>%
    transmute(CASEID, V206, V209, V364)
}
re516171_import <- function(path){
  import(path, setclass = "data.table") %>%
    setNames(., toupper(colnames(.))) %>%
    transmute(CASEID, V511, V525)
}
csalud01_import <- function(path){
  data.table(haven::read_sav( path, encoding = "latin1")) -> tmp
  colnames(tmp)[which(colnames(tmp) %in% c("Peso_may15años", "PESO15_AJUS", "PESO15AÑOS"))] <- "PESO15_AMAS"
  tmp %>%
    setNames(., toupper(colnames(.))) %>%
    transmute(HHID, PESO15_AMAS, QSSEXO, QS23,
              QS200, QS201, QS206, QS208, QS209, QS210, QS710, QS711)
}

# PISA
flt_conf <-
  (function(){
    tmp <- intsvy:::pisa2015_conf
    tmp$parameters$cutoffs <- c(0, 326.00, 400.33, 475.10, 549.86, 624.63)
    tmp$variables$pvlabelsuff <- "FLIT"
    tmp})()

pisa_flt <- function(data){
  bysexo <- intsvy.ben.pv(pvlabel = "FLIT", cutoff = c(0, 400.33), by = "sexo",
                          data = data, config = flt_conf) %>%
    select(3:4) %>% slice(c(3,6)) %>% cbind(c("MUJER", "HOMBRE"))
  colnames(bysexo) <- c("ind", "se", "desag")
  global <- intsvy.ben.pv(pvlabel = "FLIT", cutoff = c(0, 400.33),
                          data = data, config = flt_conf) %>%
    select(2:3) %>% slice(3) %>% cbind("NACIONAL")
  colnames(global) <- c("ind", "se", "desag")
  bind_rows(bysexo, global) %>% select(desag, ind, se)
}

pisa_asp <- function(data){
  bysexo <-
    data %>%
    mutate(aspira = ifelse(ST111Q01TA == 6, 1, 0)) %>%
    intsvy.table(variable = "aspira", data = ., by = "ST004D01T", config = flt_conf) %>%
    select(4:5) %>% slice(c(2,4)) %>% cbind(c("MUJER", "HOMBRE"))
  colnames(bysexo) <- c("ind", "se", "desag")
  global <-
    data %>%
    mutate(aspira = ifelse(ST111Q01TA == 6, 1, 0)) %>%
    intsvy.table(variable = "aspira", data = ., config = flt_conf) %>%
    select(3:4) %>% slice(2) %>% cbind("NACIONAL")
  colnames(global) <- c("ind", "se", "desag")
  bind_rows(bysexo, global) %>% select(desag, ind, se)
}

pisa_sat <- function(data){
  bysexo <-
    data %>%
    mutate(satisf = ifelse(ST016Q01NA == 10, 1, 0)) %>%
    intsvy.table(variable = "satisf", data = ., by = "sexo", config = flt_conf) %>%
    select(4:5) %>% slice(c(2,4)) %>% cbind(c("MUJER", "HOMBRE"))
  colnames(bysexo) <- c("ind", "se", "desag")
  global <-
    data %>%
    mutate(satisf = ifelse(ST016Q01NA == 10, 1, 0)) %>%
    intsvy.table(variable = "satisf", data = ., config = flt_conf) %>%
    select(3:4) %>% slice(2) %>% cbind("NACIONAL")
  colnames(global) <- c("ind", "se", "desag")
  bind_rows(bysexo, global) %>% select(desag, ind, se)
}

decode_direed <- function(x) {
  dd <- data.table(codigo = c("0100", "0200", "0300", "0400", "0500", 
                      "0600", "0701", "0800", "0900", "1000",
                      "1100", "1200", "1300", "1400", "1501",
                      "1502", "1600", "1700", "1800", "1900",
                      "2000", "2100", "2200", "2300", "2400", "2500"),
                   # Modificar nomenclatura original:
                   # LIMA METROPOLITANA -> LIMA PROVINCIA
                   # LIMA PROVINCIAS -> LIMA REGION
           nombre = c("AMAZONAS",  "ANCASH", "APURÍMAC", "AREQUIPA", "AYACUCHO",
                      "CAJAMARCA", "CALLAO",  "CUSCO", "HUANCAVELICA", "HUÁNUCO",
                      "ICA", "JUNÍN", "LA LIBERTAD",  "LAMBAYEQUE",
                      "LIMA PROVINCIA", "LIMA REGION", "LORETO",
                      "MADRE DE DIOS", "MOQUEGUA", "PASCO", "PIURA", "PUNO",
                      "SAN MARTÍN", "TACNA", "TUMBES", "UCAYALI"))
  dd[sapply(x, function(y)which(y==dd$codigo))]$nombre
}

c("AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO", "CAJAMARCA", 
"CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA", "JUNIN", 
"LA LIBERTAD", "LAMBAYEQUE", "LIMA REGION", "LIMA PROVINCIA", 
"LORETO", "MADRE DE DIOS", "MOQUEGUA", "PASCO", "PIURA", "PUNO", 
"SAN MARTIN", "TACNA", "TUMBES", "UCAYALI") -> regiones

###

export_ranking <- function(outfile, tabrank){
  colnames(tabrank) <-
    c("SALUD", "",
      "EDUCACION", "",
      "SEGURIDAD", "",
      "TRABAJO", "",
      "PARTICIPACION", "",
      "GLOBAL", "")
  wb <- createWorkbook(type="xlsx")
  
  # Styles
  gradient01 <- rev(union(colorRampPalette(c("green","yellow"))(50),colorRampPalette(c("yellow","red"))(52)))

  TITLE_STYLE <-
    CellStyle(wb) +
    Font(wb, heightInPoints=14,
         color="#010101", isBold=TRUE, underline=1)

  TABLE_HEADERS <- CellStyle(wb) +
    Font(wb, color = "#FEFEFE", isBold=TRUE) +
    Fill(foregroundColor="#000000", backgroundColor="#000000",
         pattern="SOLID_FOREGROUND") +
    Alignment(wrapText=TRUE, horizontal="ALIGN_LEFT") +
    Border(color="black", position=c("TOP", "BOTTOM"), 
           pen=c("BORDER_THIN", "BORDER_THIN"))

  # Diseno de hoja
  sheet <- createSheet(wb, sheetName = "Ranking")
  setZoom(sheet, numerator = 85, denominator = 100)
  
  tabrank %>%
    addDataFrame(sheet, row.names = FALSE,
                 startRow = 1,
                 startColumn = 2, 
                 colnamesStyle = TABLE_HEADERS)
    
  setColumnWidth(sheet, colIndex = 1, colWidth = 3) # 24 px
  setColumnWidth(sheet, colIndex = c(2,4,6,8,10,12),
                 colWidth = 18)
  setColumnWidth(sheet, colIndex = c(2,4,6,8,10,12)+1,
                 colWidth = 7)

  INDICADORES_CB <- CellBlock(sheet,
                              startRow = 2,
                              startColumn = 2,
                              noRows = nrow(tabrank),
                              noColumns = ncol(tabrank),
                              create = FALSE)
    
  BORDER_BODY <- Border(color = "black",
                        position = c("BOTTOM", "LEFT", "TOP", "RIGHT"),
                        pen = "BORDER_THIN")
    
  CB.setBorder(INDICADORES_CB,
               border = BORDER_BODY,
               rowIndex = rep(1:nrow(tabrank), each = ncol(tabrank)),
               colIndex = rep(1:ncol(tabrank), times = nrow(tabrank)))
    
  for(j in 1:6){
    for(i in 1:nrow(tabrank)){
      CB.setFill(INDICADORES_CB,
                 fill = Fill(foregroundColor =
                               gradient01[(tabrank[i,j*2]*
                                             100)%+rmna%1]),
                 rowIndex = i,
                 colIndex = j*2)
    }
  }
    
  xlsx::saveWorkbook(wb, outfile)
}

# Propagar nombres de fila hacia abajo
fillNAs <- function(dataf){
  for(i in 1:ncol(dataf)){
    dataf[,i] <- na.locf(dataf[,i])
  }
  dataf
}

# Seleccionar rango de edad para censo
censo_edad <- function(x, ll, ul){
  x <- x[ll<=edad&edad<=ul]
  x[x$region == "CALLAO",]$provincia <- "CALLAO"
  x[x$distrito == "ANCO_HUALLO",]$distrito <- "ANCO HUALLO"
  x[x$distrito == "RUPA-RUPA",]$distrito <- "RUPA RUPA"
  x[x$distrito == "HUAY-HUAY",]$distrito <- "HUAY HUAY"
  bind_rows(
    # GLOBAL
    x[,.(desag = "NACIONAL", pob = sum(poblacion))],
    # GLOBAL-SEXUAL
    x[,.(pob = sum(poblacion)),.(desag = sexo)],
    # REGIONAL
    x[region!="LIMA", .(pob = sum(poblacion)), .(desag = region)],
    x[region=="LIMA"&provincia=="LIMA",
      .(desag = "LIMA PROVINCIA", pob = sum(poblacion))],
    x[region=="LIMA"&provincia!="LIMA",
      .(desag = "LIMA REGION", pob = sum(poblacion))],
    # REGIONAL-SEXUAL
    x[region!="LIMA", .(pob = sum(poblacion)),
      .(desag = paste(region, sexo, sep = "_"))],
    x[region=="LIMA"&provincia=="LIMA", .(pob = sum(poblacion)),
      .(desag = paste("LIMA PROVINCIA", sexo, sep = "_"))],
    x[region=="LIMA"&provincia!="LIMA", .(pob = sum(poblacion)),
      .(desag = paste("LIMA REGION", sexo, sep = "_"))],
    # DISTRITAL
    x[region!="LIMA", .(pob = sum(poblacion)),
      .(desag = paste(region, provincia, distrito, sep = "_"))],
    x[region=="LIMA"&provincia=="LIMA", .(pob = sum(poblacion)),
      .(desag = paste("LIMA PROVINCIA", provincia, distrito, sep = "_"))],
    x[region=="LIMA"&provincia!="LIMA", .(pob = sum(poblacion)),
      .(desag = paste("LIMA REGION", provincia, distrito, sep = "_"))],
    # DISTRITAL-SEXUAL
    x[region!="LIMA", .(pob = sum(poblacion)),
      .(desag = paste(region, provincia, distrito, sexo, sep = "_"))],
    x[region=="LIMA"&provincia=="LIMA", .(pob = sum(poblacion)),
      .(desag = paste("LIMA PROVINCIA", provincia, distrito, sexo, sep = "_"))],
    x[region=="LIMA"&provincia!="LIMA", .(pob = sum(poblacion)),
      .(desag = paste("LIMA REGION", provincia, distrito, sexo, sep = "_"))]
  )
}

# Desigualdad de genero
IDG_xls <- function(sourcexl){
  sheets <- xlsx::loadWorkbook(sourcexl) %>% xlsx::getSheets() %>% names
  
  lapply(sheets,
       function(i){
         read.xlsx(sourcexl,
                   sheetName = i,
                   rowIndex = 4:9,
                   colIndex = 11:12,
                   header = FALSE) %>%
           cbind(desag = i, .) %>%
           transmute(desag, dimension = X11, valor = X12)}) %>%
    do.call(rbind, .) -> tabindex
  
  lapply(sheets,
         function(i){
           read.xlsx(sourcexl,
                     sheetName = i,
                     rowIndex = 4:27,
                     colIndex = c(2:4),
                     header = FALSE) %>%
             cbind(desag = i, .)}) %>%
    do.call(rbind, .) %>%
    filter(stri_detect_fixed(desag, "MUJER")) %>%
    transmute(desag, nombre = X3, valor = X4) -> tabindic
  
  (allindic %>%
      pull(desag) %>% 
      stri_split_fixed("_") %>%
      unlist %>%
      matrix(nrow=2))[1, ] %>%
    unique -> desagiter
  
  desagiter %>%
    lapply(function(x) list(tabindex %>%
                              filter(stri_detect_regex(desag,
                                                       paste0("^", x))),
                            tabindic %>%
                              filter(stri_detect_regex(desag,
                                                       paste0("^", x))))) %>%
    lapply(function(x) IDG_row(x[[1]], x[[2]])) %>%
    bind_rows
}

IDG_row <- function(allindex, allindic){
  allindic %>%
    filter(stri_detect_fixed(nombre, "Tasa de natalidad")) %>%
    pull(valor) %>% max(0.1) -> FE
  allindic %>%
    filter(stri_detect_fixed(nombre, "Violencia sexual")) %>%
    pull(valor) %>% max(0.1)  -> VS
  allindic %>%
    filter(stri_detect_fixed(nombre, "edad unidas")) %>%
    pull(valor) %>% max(0.1)  -> MU
  
  data.frame(
    GM =
      ((
        allindex %>%
          filter(stri_detect_fixed(desag, "MUJER") & dimension != "GLOBAL") %>%
          select(valor) %>% unlist %>% prod
      ) *
        sqrt((1/68)*1/FE)*sqrt(1/VS)*sqrt(1/MU)
      ) %>%
      `^`(1 / 7),
    GH =
      allindex %>%
      filter(stri_detect_fixed(desag,"HOMBRE") & dimension != "GLOBAL") %>%
      select(valor) %>% unlist %>% prod %>%
      `^`(1 / 7)
  ) %>%
    mutate(
      ARM = 0.5 * (1 / GM + 1 / GH),
      bSALUD =
        (allindex %>% filter(dimension == "SALUD") %>%
           pull(valor) %>% mean) + (sqrt(1 / FE) + 1) / 2,
      bEDUCA =
        allindex %>% filter(dimension == "EDUCACION") %>%
        pull(valor) %>% mean,
      bSEGUR = (
        allindex %>% filter(dimension == "SEGURIDAD") %>%
          pull(valor) %>% mean
      ) + (sqrt(1 / VS + 1 / MU) + 1) / 2,
      bTRABA =
        allindex %>% filter(dimension == "TRABAJO") %>%
        pull(valor) %>% mean,
      bPARTI =
        allindex %>% filter(dimension == "PARTICIPACION") %>%
        pull(valor) %>% mean
    ) %>%
    mutate(bGMH = (bSALUD * bEDUCA * bSEGUR * bTRABA * bPARTI) ** (1 / 5)) %>%
    mutate(IDG = 1 - ARM / bGMH)
}