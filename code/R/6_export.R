plan_export <- drake_plan (
  output_cobertura =
    write_xlsx(
      allind_table,
      "output/CoberturaIndicadores.xlsx"),
  
  output_resumen =
  write_xlsx(
    summary_table,
    "output/ResumenIndices.xlsx"),
  
  output_indicedet =
    write_xlsx(
      nicest_table,
      "output/DetalleIndices.xlsx"),
  
  output_indicadordet =
    write_xlsx(
      nice_table,
      "output/DetalleIndicadores.xlsx")
)