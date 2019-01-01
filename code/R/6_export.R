plan_export <- drake_plan (
  output_desag =
    export_all(("output/indice_nacional.xlsx"),
               tabla_normalizada %>%
                 filter(desag == "NACIONAL"),
               indices_final %>%
                 filter(desag == "NACIONAL"))
)

# loadd(tabla_normalizada)
# loadd(indices_final)