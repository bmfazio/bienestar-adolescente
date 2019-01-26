nice_plan <- drake_plan(
  nice_table =
    tabla_normalizada %>%
    separate(desag, paste0("desag",1:5), sep = "_") %>%
    transmute(region =
                case_when(
                  desag1 %in% c(hm_, ur_) ~ "NACIONAL",
                  TRUE ~ desag1),
              provincia =
                case_when(
                  desag2 %in% c(hm_, ur_) | is.na(desag2) ~ "TOTAL",
                  TRUE ~ desag2),
              distrito =
                case_when(
                  desag3 %in% c(hm_, ur_) | is.na(desag3) ~ "TOTAL",
                  TRUE ~ desag3),
              area =
                case_when(
                  desag1 %in% c(ur_) ~ desag1,
                  desag2 %in% c(ur_) ~ desag2,
                  desag3 %in% c(ur_) ~ desag3,
                  desag4 %in% c(ur_) ~ desag4,
                  desag5 %in% c(ur_) ~ desag5,
                  TRUE ~ "TOTAL"),
              sexo =
                case_when(
                  desag1 %in% c(hm_) ~ desag1,
                  desag2 %in% c(hm_) ~ desag2,
                  desag4 %in% c(hm_) ~ desag4,
                  TRUE ~ "TOTAL"),
              dimension, nombre, ind, error, peor, mejor, norm, fuente),

  ind_nacional =
    nice_table %>%
    filter(region == "NACIONAL" &
             area == "TOTAL" &
             sexo == "TOTAL"),
  
  ind_sexo =
    nice_table %>%
    filter(nombre %in%
             (nice_table %>%
                filter(region == "NACIONAL" &
                         area == "TOTAL" &
                         sexo != "TOTAL") %>%
                pull(nombre) %>% unique) &
             region == "NACIONAL" &
             area == "TOTAL"),  
  ind_area =
    nice_table %>%
    filter(nombre %in%
             (nice_table %>%
                filter(region == "NACIONAL" &
                         area != "TOTAL" &
                         sexo == "TOTAL") %>%
                pull(nombre) %>% unique) &
             region == "NACIONAL" &
             sexo == "TOTAL"),
  
  ind_sexoarea =
    nice_table %>%
    filter(nombre %in%
             (nice_table %>%
                filter(region == "NACIONAL" &
                         area != "TOTAL" &
                         sexo != "TOTAL") %>%
                pull(nombre) %>% unique) &
             region == "NACIONAL"),
  
  ind_region =
    nice_table %>%
    filter(nombre %in%
             (nice_table %>%
                filter(region != "NACIONAL" &
                         provincia == "TOTAL" &
                         area == "TOTAL" &
                         sexo == "TOTAL") %>%
                pull(nombre) %>% unique) &
             provincia == "TOTAL" &
             area == "TOTAL" &
             sexo == "TOTAL"),
  
  ind_regionsexo =
    nice_table %>%
    filter(nombre %in%
             (nice_table %>%
                filter(region != "NACIONAL" &
                         provincia == "TOTAL" &
                         area == "TOTAL" &
                         sexo != "TOTAL") %>%
                pull(nombre) %>% unique) &
             provincia == "TOTAL" &
             area == "TOTAL"),
  
  ind_regionarea =
    nice_table %>%
    filter(nombre %in%
             (nice_table %>%
                filter(region != "NACIONAL" &
                         provincia == "TOTAL" &
                         area != "TOTAL" &
                         sexo == "TOTAL") %>%
                pull(nombre) %>% unique) &
             provincia == "TOTAL" &
             sexo == "TOTAL"),
  
    
  ind_regionsexoarea =
    nice_table %>%
    filter(nombre %in%
             (nice_table %>%
                filter(region != "NACIONAL" &
                         provincia == "TOTAL" &
                         area != "TOTAL" &
                         sexo != "TOTAL") %>%
                pull(nombre) %>% unique) &
             provincia == "TOTAL"),
  
  ind_distrito =
    nice_table %>%
    filter(nombre %in%
             (nice_table %>%
                filter(distrito != "TOTAL" &
                         area == "TOTAL" &
                         sexo == "TOTAL") %>%
                pull(nombre) %>% unique) &
             sexo == "TOTAL" &
             area == "TOTAL"),
  
  ind_distritosexo =
    nice_table %>%
    filter(nombre %in%
             (nice_table %>%
                filter(distrito != "TOTAL" &
                         area == "TOTAL" &
                         sexo != "TOTAL") %>%
                pull(nombre) %>% unique) &
             area == "TOTAL"),
  
  ind_distritoarea =
    nice_table %>%
    filter(nombre %in%
             (nice_table %>%
                filter(distrito != "TOTAL" &
                         area != "TOTAL" &
                         sexo == "TOTAL") %>%
                pull(nombre) %>% unique) &
             sexo == "TOTAL"),
  
  ind_distritosexoarea =
    nice_table %>%
    filter(nombre %in%
             (nice_table %>%
                filter(distrito != "TOTAL" &
                         area != "TOTAL" &
                         sexo != "TOTAL") %>%
                pull(nombre) %>% unique))
)

nicify_plan <- drake_plan(
  subset_func =
    function(...) {
      list(...)[-1] %>%
        lapply(indic2index) %>%
        (function(x){
          subname <- names(x)
          for(i in subname)x[[i]]<-x[[i]] %>% mutate(subtipo = i)
          x
        }) %>%
        bind_rows()
    }
)

nicer_plan <- gather_plan(
  plan = nice_plan,
  target = "nicer_table",
  gather = "subset_func"
)

nicest_plan <- drake_plan(
  nicest_table =
    nicer_table %>%
    spread(dimension, indice) %>%
    select(region, provincia, distrito,
           area, sexo, subtipo,
           SALUD, EDUCACION,
           SEGURIDAD, TRABAJO,
           PARTICIPACION, GLOBAL) %>% ungroup,
  
  summary_table =
    nicest_table %>%
    filter(subtipo == "ind_nacional") %>%
    select(region, Global = GLOBAL) %>%
    left_join(
      nicest_table %>%
        filter(subtipo == "ind_sexo",
               sexo != "TOTAL") %>%
        select(region, sexo, GLOBAL) %>%
        spread(sexo, GLOBAL) %>%
    select(region, Hombre = HOMBRE, Mujer = MUJER),
    by = "region") %>%
    left_join(
      nicest_table %>%
        filter(subtipo == "ind_area",
               area != "TOTAL") %>%
        select(region, area, GLOBAL) %>%
        spread(area, GLOBAL) %>%
    select(region, Urbano = URBANA, Rural = RURAL),
    by = "region") %>%
    bind_rows(
      nicest_table %>%
        filter(subtipo == "ind_region") %>%
        select(region, Global = GLOBAL) %>%
        left_join(
          nicest_table %>%
            filter(subtipo == "ind_regionsexo",
                   sexo != "TOTAL") %>%
            select(region, sexo, GLOBAL) %>%
            spread(sexo, GLOBAL) %>%
            select(region, Hombre = HOMBRE, Mujer = MUJER),
          by = "region") %>%
        left_join(
          nicest_table %>%
            filter(subtipo == "ind_regionarea",
                   area != "TOTAL") %>%
            select(region, area, GLOBAL) %>%
            spread(area, GLOBAL) %>%
            select(region, Urbano = URBANA, Rural = RURAL),
          by = "region")        
    )
)

plan_wrangle <- bind_plans(
  nice_plan,
  nicify_plan,
  nicer_plan
)

# presentar una tabla con los indicadores para cada desagregacion:
# lista completa de indicadores en una columna ('nombre')
# columnas adicionales indicando cada desagregacion y cuales indicadores retiene