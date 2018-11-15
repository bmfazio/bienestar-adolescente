source(file.path("drake","setup.R"))
source(file.path("drake","dataprep.R"))
source(file.path("drake","indicators.R"))

fplan <- bind_plans(
  plan_prep,
  indicators_all
)

make(fplan)

vis_drake_graph(drake_config(fplan))