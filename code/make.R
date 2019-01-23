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

# x <- "D:/gitrepos/bienestar_adolescente-unicef/code/output/indice_NacionalSexo.xlsx"
# x <- "D:/gitrepos/bienestar_adolescente-unicef/code/output/indice_RegionalSexo.xlsx"
# 
# IDG_xls
# 
# 
# # para cada desagregacion: calcular productoria, agregar indic sueltos, botar dataframe con columnas desag, indice hombre, indice mujer, indice desigualdad
# 
#     (function(x){
#       sheets <-
#         xlsx::loadWorkbook(x) %>%
#         xlsx::getSheets() %>% names
#   
#       lapply(sheets,
#              function(i){
#                read.xlsx(x,
#                          sheetName = i,
#                          rowIndex = 4:9,
#                          colIndex = 11:12,
#                          header = FALSE) %>%
#                  cbind(desag = i, .) %>%
#                  transmute(desag, dimension = X11, valor = X12)}) %>%
#         do.call(rbind, .) -> allindex
#       cbind(
#         allindex %>%
#           filter(dimension == "SALUD") %>%
#           arrange(-valor) %>%
#           select(-dimension),
#       allindex %>%
#         filter(dimension == "EDUCACION") %>%
#         arrange(-valor) %>%
#         select(-dimension),
#       allindex %>%
#         filter(dimension == "SEGURIDAD") %>%
#         arrange(-valor) %>%
#         select(-dimension),
#       allindex %>%
#         filter(dimension == "TRABAJO") %>%
#         arrange(-valor) %>%
#         select(-dimension),
#       allindex %>%
#         filter(dimension == "PARTICIPACION") %>%
#         arrange(-valor) %>%
#         select(-dimension),
#       allindex %>%
#         filter(dimension == "GLOBAL") %>%
#         arrange(-valor) %>%
#         select(-dimension)
#       )})(file_in("output/indice_tablas.xlsx"))