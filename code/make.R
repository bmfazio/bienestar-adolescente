options(encoding = "utf8")
source(file.path("R","1_setup.R"))
source(file.path("R","2_dataprep.R"))
source(file.path("R","3_indicators.R"))
source(file.path("R","4_dimensions.R"))
source(file.path("R","6_export.R"))

plan <- bind_plans(
  plan_prep,
  plan_indicators,
  plan_dimensions,
  plan_export
)

make(plan)

#vis_drake_graph(drake_config(plan))

# ARREGLAR empalmes con censo (nombres desag)

# loadd(tabla_normalizada)