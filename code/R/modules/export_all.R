# Styles
gradient01 <- rev(union(colorRampPalette(c("green","yellow"))(50),colorRampPalette(c("yellow","red"))(52)))

export_all <- function(outfile, input_indicadores, tab_model, repnames = NULL){
  wb <- createWorkbook(type="xlsx")

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
    indicTable <- tab_model %>%
      left_join(
        input_indicadores %>%
          subset(desag == val_desagrega),
        by = "nombre") %>%
      mutate(ind.y = round(ind.y, 1),
             error.y = round(error.y, 1),
             norm.y = round(norm.y, 2),
             mejor.x = round(mejor.x, 1),
             peor.x = round(peor.x, 1)
             ) %>%
      select(`Dimensión` = dimension.x,
             `Indicador` = nombre,
             `Valor` = ind.y,
             `+/-` = error.y,
             `Mejor` = mejor.x,
             `Peor` = peor.x,
             `Norm.` = norm.y,
             `Fuente` = fuente.x)
    
    # Asumo que si no aparece cierta zona en depresion o mortalidad
    # es porque no tuvo casos -> ponerle 0 al indicador (1 en normalizado)
    if(any(is.na(indicTable[indicTable$Indicador ==
                            "Prevalencia de depresión" &
                            is.na(indicTable$Valor), c(3,4,7)]))){
      indicTable[indicTable$Indicador ==
                   "Prevalencia de depresión" &
                   is.na(indicTable$Valor), c(3,4,7)] <- c(0,0,1)
      
    }
    if(any(is.na(indicTable[indicTable$Indicador ==
                            "Tasa de mortalidad global" &
                            is.na(indicTable$Valor), c(3,4,7)]))){
      indicTable[indicTable$Indicador ==
                   "Tasa de mortalidad global" &
                   is.na(indicTable$Valor), c(3,4,7)] <- c(0,0,1)
    }
    ###
    ###
        
    indicTable %>%
      addDataFrame(sheet, row.names = FALSE,
                   startRow = posicion_tablas[1],
                   startColumn = posicion_tablas[2], 
                   colnamesStyle = TABLE_HEADERS)
    
    # Calcular los indices, ignorando indicadores sin información
    # ... a menos que no haya ni un solo indicador, en cuyo caso
    # se estaria asumiendo que la dimension es cero
    tmp.indices <- indicTable %>%
      group_by(`Dimensión`) %>%
      summarize(indice = mean(`Norm.`, na.rm = TRUE))
    
    indexTable <- tmp.indices %>%
    summarize(indice =
                exp(mean(log(
                  ifelse(indice < 0.01, 0.01, indice)
                  )))) %>%
    cbind(`Dimensión` = "GLOBAL") %>%
    bind_rows(tmp.indices) %>%
    arrange(
      match(`Dimensión`,
            c("SALUD",
              "EDUCACION",
              "SEGURIDAD",
              "TRABAJO",
              "PARTICIPACION",
              "GLOBAL"))
      ) %>%
      mutate(indice = round(indice, 2)) %>%
      select(`Dimensión`,
             `Índice` = indice)
    
    indexTable %>%
    addDataFrame(sheet, row.names = FALSE,
                 startRow = posicion_tablas[1],
                 startColumn = posicion_tablas[2] + ncol(indicTable) + 1, 
                 colnamesStyle = TABLE_HEADERS
                 )
    
    # Aplicar formatos
      # Poner titulo a cada hoja
    xlsx.addTitle(sheet,
                  rowI = 1, colI = 2,
                  title = ifelse(is.null(repnames),
                                 val_desagrega,
                                 repnames %>%
                                   filter(ubigeo == substr(val_desagrega,1,4)) %>%
                                   transmute(a = paste(depa, prov,
                                                       substr(val_desagrega, 6, 99))) %>%
                                   pull(a)
                                   ),
                  titleStyle = TITLE_STYLE)
    
    setColumnWidth(sheet, colIndex = 1, colWidth = 3)
    setColumnWidth(sheet, colIndex = 2, colWidth = 15)
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
      coltmp <- indicTable[i,7]
      CB.setFill(INDICADORES_CB,
                 fill = Fill(foregroundColor =
                               ifelse(is.na(coltmp),
                                      "#A0A0A0",
                                      gradient01[(coltmp*100)+1])),
                 rowIndex = i,
                 colIndex = 7)
    }
    
    CB.setBorder(INDICES_CB,
                 border = BORDER_BODY,
                 rowIndex = rep(1:nrow(indexTable), each = 2),
                 colIndex = rep(1:2, times = nrow(indexTable)))
    CB.setFont(INDICES_CB, Font(wb, isBold = T),
               rowIndex = c(nrow(indexTable),nrow(indexTable)),
               colIndex = 1:2)
    
    for(i in 1:nrow(indexTable)){
      CB.setFill(INDICES_CB,
                 fill = Fill(foregroundColor = gradient01[indexTable[i,2]*100+1]),
                 rowIndex = i,
                 colIndex = 2)
    }
  }
  xlsx::saveWorkbook(wb, outfile)
}
