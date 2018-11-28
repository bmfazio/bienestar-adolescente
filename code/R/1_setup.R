library(rio)
library(yaml)
library(xlsx)
library(dplyr)
library(drake)
library(readxl)
library(ggplot2)
library(survey)
library(stringi)
library(data.table)

options(stringsAsFactors = FALSE)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

datadir <- read_yaml("config.yaml")[[paste(Sys.info(),collapse="|")]]

# ENDES subdir
if(length(datadir) == 1 & any(dir.exists(datadir))){
  ineidir <- file.path(datadir, "inei")
  minedudir <- file.path(datadir, "minedu")
} else {
  stop("data dir not found")
}

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

# Normalize indicators
normind <- function(x, max, min) {
  xn <- (x - min)/(max - min)
  case_when(
    xn > 1 ~ 1,
    xn < 0 ~ 0,
    TRUE ~ xn
  )
}

# Radar plot
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}
RadarTheme <- theme(panel.background=element_blank(),
                    plot.title= element_text(size = 20,face=c("bold","italic")),
                    plot.margin = unit(rep(2.5,4), "cm"),
                    aspect.ratio = 1,
                    legend.position="bottom",
                    legend.title=element_blank(),
                    legend.direction="vertical",
                    strip.text.x = element_text(size = rel(0.8)),
                    axis.text.x = element_text(size = 12,face ="bold"),
                    axis.ticks.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.line.x=element_line(size=0.5),
                    panel.grid.major=element_line(size=0.3,linetype = 2,colour="grey"))

# Custom svy processing
# FALTA: cambiar nombres de columna para poder unir todo con mas facilidad
svy_mean <- function(svyobj, desag, formulind){
  desagr <- svyby(formulind, desag, design = svyobj, svymean, na.rm = T)
  colnames(desagr) <- c("desag", "ind", "se")
  global <- svyby(formulind, ~as.factor("NACIONAL"), design = svyobj, svymean, na.rm = T)
  colnames(global) <- c("desag", "ind", "se")
  bind_rows(desagr, global)

}

svy_prop <- function(svyobj, desag, formulind){
  desagr <- svyby(formulind, desag, design = svyobj, svyciprop, na.rm = T)
  colnames(desagr) <- c("desag", "ind", "se")
  global <- svyby(formulind, ~as.factor("NACIONAL"), design = svyobj, svyciprop, na.rm = T)
  colnames(global) <- c("desag", "ind", "se")
  bind_rows(desagr, global)
}

# tabladores
tabfun <- function(source, name, scale = 100) {
  cbind(
    nombre = name,
    source %>%
      mutate(ind = ind*scale, error = 1.96*se*scale) %>%
      select(-se)
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

## ecsel
xlsx.addTitle <- function(sheet, rowI, title, titleStyle, colI = 1){
  rows <-createRow(sheet, rowIndex=rowI)
  sheetTitle <- createCell(rows, colIndex=colI)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}
export_all <- function(outfile, input_indicadores, input_indices){
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

  for(val_desagrega in unique(input_indicadores$desag)){
    # Diseno de hoja
    sheet <- createSheet(wb, sheetName = val_desagrega)
    setZoom(sheet, numerator = 85, denominator = 100)
    posicion_tablas <- c(3, 2, 1)

    # Poner data
    indicTable <- input_indicadores %>%
      subset(desag == "NACIONAL") %>%
      left_join(
        input_indicadores %>%
          subset(desag == val_desagrega),
        by = "nombre") %>%
      mutate(ind.y = round(ind.y, 1),
             error.y = round(error.y, 1),
             norm.y = round(norm.y, 2)
             ) %>%
      select(`Dimensión` = dimension.x,
             `Indicador` = nombre,
             `Valor` = ind.y,
             `+/-` = error.y,
             `L inf` = upper.x,
             `L sup` = lower.x,
             `Norm.` = norm.y,
             `Fuente` = fuente.x)
    
    indicTable %>%
      addDataFrame(sheet, row.names = FALSE,
                   startRow = posicion_tablas[1],
                   startColumn = posicion_tablas[2], 
                   colnamesStyle = TABLE_HEADERS)
    
    indexTable <- input_indices %>%
      subset(desag == val_desagrega) %>%
      mutate(indice = round(indice, 2)) %>%
      select(`Dimensión` = dimension,
             `Índice` = indice)
    
    indexTable %>%
    addDataFrame(sheet, row.names = FALSE,
                 startRow = posicion_tablas[1],
                 startColumn = posicion_tablas[2] + ncol(indicTable) + 1, 
                 colnamesStyle = TABLE_HEADERS
                 )
    
    # Aplicar formatos
    xlsx.addTitle(sheet,
                  rowI = 1, colI = 2,
                  title = val_desagrega, titleStyle = TITLE_STYLE)
    
    setColumnWidth(sheet, colIndex = 1, colWidth = 3) # 24 px
    setColumnWidth(sheet, colIndex = 2, colWidth = 15) #108 px
    setColumnWidth(sheet, colIndex = 3, colWidth = 70.5)
    setColumnWidth(sheet, colIndex = 4:8, colWidth = 7)
    setColumnWidth(sheet, colIndex = 9, colWidth = 14)
    setColumnWidth(sheet, colIndex = 10, colWidth = 3)
    setColumnWidth(sheet, colIndex = 11, colWidth = 15)
    setColumnWidth(sheet, colIndex = 12, colWidth = 7)
    
    INDICADORES_CB <- CellBlock(sheet,
                                startRow = posicion_tablas[1]+1,
                                startColumn = posicion_tablas[2],
                                noRows = nrow(indicTable),
                                noColumns = ncol(indicTable),
                                create = FALSE)
    
    INDICES_CB <- CellBlock(sheet,
                            startRow = posicion_tablas[1]+1,
                            startColumn = posicion_tablas[2] + ncol(indicTable) + 1,
                            noRows = nrow(indicTable),
                            noColumns = ncol(indicTable),
                            create = FALSE)
    
    BORDER_BODY <- Border(color = "black",
                          position = c("BOTTOM", "LEFT", "TOP", "RIGHT"),
                          pen = "BORDER_THIN")
    
    CB.setBorder(INDICADORES_CB,
                 border = BORDER_BODY,
                 rowIndex = rep(1:nrow(indicTable), each = ncol(indicTable)),
                 colIndex = rep(1:ncol(indicTable), times = nrow(indicTable)))
    for(i in 1:nrow(indicTable)){
      CB.setFill(INDICADORES_CB,
                 fill = Fill(foregroundColor =
                               gradient01[(indicTable[i,7]*100)%+rmna%1]),
                 rowIndex = i,
                 colIndex = 7)
    }
    
    CB.setBorder(INDICES_CB,
                 border = BORDER_BODY,
                 rowIndex = rep(1:6, each = 2),
                 colIndex = rep(1:2, times = 6))
    CB.setFont(INDICES_CB, Font(wb, isBold = T),
               rowIndex = c(6,6), colIndex = 1:2)
    
    for(i in 1:6){
      CB.setFill(INDICES_CB,
                 fill = Fill(foregroundColor = gradient01[indexTable[i,2]*100]),
                 rowIndex = i,
                 colIndex = 2)
    }
  }
  xlsx::saveWorkbook(wb, outfile)
}