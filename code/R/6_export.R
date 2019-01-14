plan_export1 <- drake_plan (
  output_prueba =
    export_all(file_out("output/indice_tablas.xlsx"),
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
  
  output_nacional_3 =
    export_all(("output/indice_IG3.xlsx"),
               tabla_normalizada %>% filter(desag == "NACIONAL"),
               tabla_normalizada %>% filter(desag == "NACIONAL" & nombre %in%
                                              c("Prevalencia de depresión",
                                                "Tasa de mortalidad global",
                                                "% con competencia satisfactoria en lectura",
                                                "% con competencia satisfactoria en matemática"))),

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
  
  output_regional_2 =
    export_all(("output/indice_IGR2.xlsx"),
               tabla_normalizada %>%
                 filter(stri_count_fixed(desag, "_") == 0 &
                          !(desag %in% c("HOMBRE", "MUJER", "NACIONAL"))),
               tabla_normalizada %>% filter(desag == "NACIONAL" & nombre %in%
                                              c("Prevalencia de depresión",
                                                "Tasa de mortalidad global",
                                                "% con competencia satisfactoria en lectura",
                                                "% con competencia satisfactoria en matemática"))),
  
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
        transmute(ubigeo = substr(ubigeo, 1, 4), regprov) %>% unique,
      by = "regprov") %>%
    transmute(desag =
                stri_split_fixed(desag, "_") %>%
                sapply(function(x)x[3]) %>%
                paste0(ubigeo,"_",.) %>%
                substr(1, 30),
              dimension, nombre, ind, error, peor, mejor, norm, fuente),
  tabla_ranking =
    (function(x){
      sheets <-
        xlsx::loadWorkbook(x) %>%
        xlsx::getSheets() %>% names
  
      lapply(sheets,
             function(i){
               read.xlsx(x,
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
      )})(file_in("output/indice_tablas.xlsx")),
  
  output_ranking =
    export_ranking("output/ranking.xlsx", tabla_ranking),
  
  output_sexo =
    export_all(file_out("output/indice_NacionalSexo.xlsx"),
               rbind(
                 tabla_normalizada %>%
                   filter(desag == "HOMBRE" & nombre != "Violencia sexual ejercida por otra persona que no es su pareja"),
                 tabla_normalizada %>%
                   filter(desag == "MUJER" | (desag == "NACIONAL" & nombre %in% c("Tasa de natalidad adolescente", "Menores de edad unidas"))) %>% mutate(desag = "MUJER")),
               tabla_normalizada %>% filter(desag == "NACIONAL" & !(nombre %in% c("Prevalencia de desnutrición o sobrepeso", "Prevalencia de anemia", "Proporción de adolescentes que usó condón en último encuentro sexual","Proporción de adolescentes sexualmente activos con método moderno","% adolescentes que han experimentado violencia por parte de su pareja","% adolescentes involucrados en trabajo infantil (c/tiempo hogar)")))),
  output_sexo2 =
    export_all(file_out("output/indice_RegionalSexo.xlsx"),
               tabla_normalizada %>% 
                 filter(sapply(desag,function(x)
                   stri_count_fixed(x, "_")) == 0) %>%
                 select(desag) %>% unlist %>%
                 setdiff(c("NACIONAL", "HOMBRE", "MUJER")) %>%
                 lapply(function(x){
                   tabla_normalizada %>%
                     filter(desag == x |
                              (stri_count_fixed(desag, "_") == 1 &
                                 stri_detect_regex(desag, paste0("^",x)))) -> global
    
                   rbind(
                     global %>%
                       filter(stri_detect_fixed(desag, "HOMBRE") &
                                nombre != "Violencia sexual ejercida por otra persona que no es su pareja") %>%
                       mutate(desag = "HOMBRE"),
                     global %>%
                       filter(stri_detect_fixed(desag, "MUJER") |
                                nombre %in% c("Tasa de natalidad adolescente", "Menores de edad unidas")) %>%
                       mutate(desag = "MUJER")
                     ) %>% mutate(desag = paste0(x, "_", desag))
                   }) %>%
                 bind_rows(
                   tabla_normalizada %>%
                     filter(desag == "HOMBRE" & nombre != "Violencia sexual ejercida por otra persona que no es su pareja"),
                   tabla_normalizada %>%
                     filter(desag == "MUJER" | (desag == "NACIONAL" & nombre %in% c("Tasa de natalidad adolescente", "Menores de edad unidas"))) %>% mutate(desag = "MUJER")),
               tabla_normalizada %>% filter(desag == "NACIONAL" & !(nombre %in% c("Prevalencia de desnutrición o sobrepeso", "Prevalencia de anemia", "Proporción de adolescentes que usó condón en último encuentro sexual","Proporción de adolescentes sexualmente activos con método moderno","% adolescentes que han experimentado violencia por parte de su pareja","% adolescentes involucrados en trabajo infantil (c/tiempo hogar)", "% que aspira a educación superior", "% completamenta satisfecho con su vida", "% con conocimiento financiero suficiente","% adolescentes en actividades recreacionales por un periodo específico","% que confía mucho o plenamente en el gobierno nacional","% que manifiesta que sus profesores los motivan con frecuencia a expresar sus opiniones"))))
)

plan_export2 <- tibble(
  target =
    sprintf("%02d",1:25) %>%
    (function(x){
      paste0("output/indice_ID1_",x)
    }),
  command =
    sprintf("%02d",1:25) %>%
    (function(x){
      paste0(
'export_all(("output/ID/indice_ID1_',
c(regiones[1:14], "LIMA", regiones[-(1:16)])[as.numeric(x)],
'.xlsx"),
tabla_ubidist %>% filter(substr(desag,1,2) == "',x,'"),
tabla_normalizada %>% filter(desag == "NACIONAL" & nombre %in% c("Prevalencia de depresión", "Tasa de mortalidad global", "% con competencia satisfactoria en lectura", "% con competencia satisfactoria en matemática")),
ubigeo2 %>% transmute(ubigeo = substr(ubigeo,1,4), depa, prov) %>% unique)')
    })
)

plan_export <- bind_plans(
  plan_export1, plan_export2
)