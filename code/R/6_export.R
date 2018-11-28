plan_export <- drake_plan (
  output_desag =
    export_all(file_out("output/desagregados.xlsx"),
               tabla_normalizada, indices_final)
  # output_table =
  #   write.xlsx(list(
  #     Salud =
  #       tabla_salud,
  #     Educacion =
  #       tabla_educacion,
  #     Seguridad =
  #       tabla_seguridad,
  #     Trabajo =
  #       tabla_trabajo,
  #     Participacion =
  #       tabla_participacion,
  #     Indice =
  #       tabla_indice), 
  #     file_out("output/indice_adolescente.xlsx"))
  # 
)