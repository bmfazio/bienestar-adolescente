# Load libraries and custom functions
source(file.path("drake","setup.R"))
source(file.path("drake","prep_endes.R"))
source(file.path("drake","prep_enaho.R"))
source(file.path("drake","prep_ece.R"))

plan <- bind_plans(
  ece_load,
  enaho_load,
  endes_load,
  enaho_merged,
  endes_merged
)

make(plan)