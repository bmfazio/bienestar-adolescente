source(file.path("drake","prep_endes.R"))
source(file.path("drake","prep_enaho.R"))
source(file.path("drake","prep_ece.R"))
source(file.path("drake","prep_enares.R"))

plan_prep <- bind_plans(
  endes_load,
  endes_merge,
  enaho_load,
  enaho_merge,
  enares_load,
  ece_load
)