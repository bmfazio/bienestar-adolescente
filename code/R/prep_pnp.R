pnp_load <- drake_plan(
  pnp.violencia =
    fread(
      file_in("DATADIR__/pnp/observatorio_violencia.csv")) %>%
    mutate(Edad = Edad %>% substr(1,2) %>% as.numeric,
           Departamento = ifelse(Departamento == "CUZCO", "CUSCO", Departamento))
) %>%
  evaluate_plan(
    rules = list(DATADIR__ = datadir),
    expand = FALSE
)