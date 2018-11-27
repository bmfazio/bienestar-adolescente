plan_export <- drake_plan (
  output_table =
    write.xlsx(list(
      Salud =
        tabla_salud,
      Educacion =
        tabla_educacion,
      Seguridad =
        tabla_seguridad,
      Trabajo =
        tabla_trabajo,
      Participacion =
        tabla_participacion,
      Indice =
        tabla_indice), 
      file_out("output/indice_adolescente.xlsx"),
      starCol = 3, startRow = 2
      )
  
)