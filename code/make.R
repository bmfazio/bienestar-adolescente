options(encoding = "utf8")
source(file.path("R","1_setup.R"))
source(file.path("R","2_dataprep.R"))
source(file.path("R","3_indicators.R"))
source(file.path("R","4_dimensions.R"))
source(file.path("R","5_index.R"))
source(file.path("R","6_export.R"))

plan <- bind_plans(
  plan_prep,
  plan_indicators,
  plan_dimensions,
  plan_index,
  plan_export
)

make(plan)

vis_drake_graph(drake_config(plan))

# loadd(indices_final)
# names(indices_final)
# 
# cbind(
# indices_final %>%
#   filter(dimension == "SALUD" &
#            !(desag %in% c("HOMBRE", "MUJER"))) %>%
#   select(-dimension) %>%
#   arrange(-indice),
# 
# indices_final %>%
#   filter(dimension == "EDUCACION" &
#            !(desag %in% c("HOMBRE", "MUJER"))) %>%
#   select(-dimension) %>%
#   arrange(-indice),
# 
# indices_final %>%
#   filter(dimension == "SEGURIDAD" &
#            !(desag %in% c("HOMBRE", "MUJER"))) %>%
#   select(-dimension) %>%
#   arrange(-indice),
# 
# indices_final %>%
#   filter(dimension == "TRABAJO" &
#            !(desag %in% c("HOMBRE", "MUJER"))) %>%
#   select(-dimension) %>%
#   arrange(-indice),
# 
# indices_final %>%
#   filter(dimension == "PARTICIPACION" &
#            !(desag %in% c("HOMBRE", "MUJER"))) %>%
#   select(-dimension) %>%
#   arrange(-indice),
# 
# indices_final %>%
#   filter(dimension == "GLOBAL" &
#            !(desag %in% c("HOMBRE", "MUJER"))) %>%
#   select(-dimension) %>%
#   arrange(-indice)) %>% write.xlsx("output/ranking.xlsx")