source(file.path("R","1_setup.R"))
source(file.path("R","2_dataprep.R"))
source(file.path("R","3a_indicators.R"))
source(file.path("R","4a_dimensions.R"))
source(file.path("R","5a_index.R"))
source(file.path("R","6_export.R"))

plan <- bind_plans(
  plan_prep,
  plan_indicators,
  plan_dimensions,
  plan_index
  # plan_export
)

make(plan)

vis_drake_graph(drake_config(plan))