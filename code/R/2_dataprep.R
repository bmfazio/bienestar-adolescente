source(file.path("R","prep_endes.R"))
source(file.path("R","prep_enaho.R"))
source(file.path("R","prep_enares.R"))
source(file.path("R","prep_enut.R"))
source(file.path("R","prep_eti.R"))
source(file.path("R","prep_ece.R"))
source(file.path("R","prep_pisa.R"))
source(file.path("R","prep_pnp.R"))

plan_prep <- bind_plans(
  endes_load,
  endes_merge,
  enaho_load,
  enaho_merge,
  enares_load,
  enut_load,
  enut_merge,
  eti_load,
  ece_load,
  pisa_plan,
  pnp_load
)