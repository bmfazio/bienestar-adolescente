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
  saveWorkbook(wb, outfile)
}
export_all("uhmm.xlsx", tabla_normalizada, indices_final)