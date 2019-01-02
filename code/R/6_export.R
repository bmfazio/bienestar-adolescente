plan_export1 <- drake_plan (
  output_prueba =
    export_all(("output/indice_tablas.xlsx"),
               tabla_normalizada %>%
                 filter(stri_count_fixed(desag, "_") == 0 &
                          !(desag %in% c("HOMBRE", "MUJER"))) %>%
                 bind_rows(
                   tabla_normalizada %>%
                     filter(desag == "NACIONAL" &
                              !(nombre %in%
                                  c("% que aspira a educación superior",
                                    "% completamenta satisfecho con su vida",
                                    "% con conocimiento financiero suficiente",
                                    "% adolescentes en actividades recreacionales por un periodo específico",
                                    "% que confía mucho o plenamente en el gobierno nacional",
                                    "% que manifiesta que sus profesores los motivan con frecuencia a expresar sus opiniones"))) %>%
                     mutate(desag = "NACIONAL 2")
                 ),
               tabla_normalizada %>% filter(desag == "NACIONAL")),
  output_nacional =
    export_all(("output/indice_IG1.xlsx"),
               tabla_normalizada %>% filter(desag == "NACIONAL"),
               tabla_normalizada %>% filter(desag == "NACIONAL")),
  
  output_regional =
    export_all(("output/indice_IGR1.xlsx"),
               tabla_normalizada %>%
                 filter(stri_count_fixed(desag, "_") == 0 &
                          !(desag %in% c("HOMBRE", "MUJER", "NACIONAL"))),
               tabla_normalizada %>%
                 filter(desag == "NACIONAL" & !(nombre %in%
                          c("% que aspira a educación superior",
                            "% completamenta satisfecho con su vida",
                            "% con conocimiento financiero suficiente",
                            "% adolescentes en actividades recreacionales por un periodo específico",
                            "% que confía mucho o plenamente en el gobierno nacional",
                            "% que manifiesta que sus profesores los motivan con frecuencia a expresar sus opiniones")))),
  
  output_nacional_2 =
    export_all(("output/indice_IG2.xlsx"),
               tabla_normalizada %>% filter(desag == "NACIONAL"),
               tabla_normalizada %>%
                 filter(desag == "NACIONAL" & !(nombre %in%
                          c("% que aspira a educación superior",
                            "% completamenta satisfecho con su vida",
                            "% con conocimiento financiero suficiente",
                            "% adolescentes en actividades recreacionales por un periodo específico",
                            "% que confía mucho o plenamente en el gobierno nacional",
                            "% que manifiesta que sus profesores los motivan con frecuencia a expresar sus opiniones")))),
  
  tabla_ubidist =
    tabla_normalizada %>%
    filter(stri_count_fixed(desag, "_") == 2 &
             !(stri_detect_regex(desag,
                                 pattern = c("_HOMBRE|_MUJER")))) %>%
    mutate(regprov =
             stri_split_fixed(desag, "_") %>%
             sapply(function(x){
               x[1] <- ifelse(x[1] %>%
                                stri_detect_fixed("LIMA"), "LIMA", x[1])
               paste(x[1], x[2], sep = "_")
               })) %>%
    left_join(
      ubigeo %>%
        mutate(regprov =
                 stri_split_fixed(desag, "_") %>%
                 sapply(function(x)paste(x[1], x[2], sep = "_"))) %>%
        transmute(ubigeo = substr(ubigeo, 1, 4), regprov),
      by = "regprov") %>%
    transmute(desag =
                stri_split_fixed(desag, "_") %>%
                sapply(function(x)x[3]) %>%
                paste0(ubigeo,"_",.) %>%
                substr(1, 30),
              dimension, nombre, ind, error, peor, mejor, norm, fuente),
  tabla_ranking =
    (function(){
      sheets <-
        xlsx::loadWorkbook("output/indice_tablas.xlsx") %>%
        xlsx::getSheets() %>% names
  
      lapply(sheets,
             function(i){
               read.xlsx("output/indice_tablas.xlsx",
                         sheetName = i,
                         rowIndex = 4:9,
                         colIndex = 11:12,
                         header = FALSE) %>%
                 cbind(desag = i, .) %>%
                 transmute(desag, dimension = X11, valor = X12)}) %>%
        do.call(rbind, .) -> allindex
      cbind(
        allindex %>%
          filter(dimension == "SALUD") %>%
          arrange(-valor) %>%
          select(-dimension),
      allindex %>%
        filter(dimension == "EDUCACION") %>%
        arrange(-valor) %>%
        select(-dimension),
      allindex %>%
        filter(dimension == "SEGURIDAD") %>%
        arrange(-valor) %>%
        select(-dimension),
      allindex %>%
        filter(dimension == "TRABAJO") %>%
        arrange(-valor) %>%
        select(-dimension),
      allindex %>%
        filter(dimension == "PARTICIPACION") %>%
        arrange(-valor) %>%
        select(-dimension),
      allindex %>%
        filter(dimension == "GLOBAL") %>%
        arrange(-valor) %>%
        select(-dimension)
      )})(),
  
  output_ranking =
    export_ranking("output/ranking.xlsx", tabla_ranking)
    
)

# plan_export2 <- tibble(
#   target =
#     sprintf("%02d",1:25) %>%
#     (function(x){
#       paste0("output/indice_ID1_",x)
#     }),
#   command =
#     sprintf("%02d",1:25) %>%
#     (function(x){
#       paste0(
# 'export_all(("output/indice_ID1_',x,'.xlsx"),
# tabla_ubidist %>% filter(substr(desag,1,2) == ',x,'),
# tabla_normalizada %>% filter(desag == "NACIONAL"))')
#     })
# )

plan_export <- bind_plans(
  plan_export1#, plan_export2
)
