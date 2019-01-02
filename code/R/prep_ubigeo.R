ubigeo_load <- drake_plan(
  ubigeo =
    fread(
      file_in("DATADIR__/ubigeo.csv")) %>%
    mutate(prov = case_when(prov == "PROV. CONST. DEL CALLAO" ~ "CALLAO",
                            prov == "NASCA" ~ "NAZCA",
                            prov == "ANTONIO RAYMONDI" ~ "ANTONIO RAIMONDI",
                            prov == "CARLOS FERMIN FITZCARRALD" ~ "CARLOS FERMIN FITZCARRAL",
                            prov == "VILCAS HUAMAN" ~ "VILCASHUAMAN",
                            TRUE ~ prov)) %>%
    transmute(ubigeo, desag = paste(depa, prov, dist, sep = "_"))
) %>%
  evaluate_plan(
    rules = list(DATADIR__ = datadir),
    expand = FALSE
)