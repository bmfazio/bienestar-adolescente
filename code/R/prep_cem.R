cem_load <- drake_plan(
  cem17 =
    import(
      file_in("DATADIR__/cem/CONSOLIDADO CASOS - DICIEMBRE 2017.sav"),
    setclass = "data.table") %>%
    mutate(v_psi =
             any(GRITOS_INSULTOS,
                 VIOLENCIA_RACIAL,
                 INDIFERENCIA,
                 DISCR_ORIENTACION_SEXUAL,
                 DISCR_GENERO,
                 DISCR_IDENTIDAD_GENERO,
                 RECHAZO,
                 DESVALORIZACION_HUMILLACION,
                 AMENAZA_QUITAR_HIJOS,
                 OTRAS_AMENAZAS,
                 PROHIBE_RECIBIR_VISITAS,
                 PROHIBE_ESTUDIAR_TRABAJAR_SALIR,
                 ROMPE_DESTRUYE_COSAS,
                 VIGILANCIA_CONTINUA_PERSECUCION,
                 BOTAR_CASA,
                 AMENAZA_DE_MUERTE,
                 ABANDONO,
                 OTRA_VPSI),
           v_fis =
             any(PUNTAPIES_PATADAS,
                 `PUÑETAZOS`,
                 BOFETADAS,
                 JALONES_CABELLO,
                 OTRAS_AGRESIONES,
                 EMPUJONES,
                 GOLPES_CON_PALOS,
                 LATIGAZO,
                 AHORCAMIENTO,
                 HERIDAS_CON_ARMAS,
                 GOLPES_CON_OBJETOS_CONTUNDENTES,
                 NEGLIGENCIA,
                 OTRA_VFIS),
           v_sex =
             any(ACOSO_SEXUAL,
                 OFENSAS_AL_PUDOR,
                 VIOLACION,
                 ACTOS_CONTRA_EL_PUDOR,
                 TRATA_CON_FINES_EXPLOTACION_SEXUAL,
                 EXPLOTACION_SEXUAL,
                 PORNOGRAFIA,
                 OTRA_VSEX))  %>%
    #adolescentes
    filter(12<=EDAD_VICTIMA&EDAD_VICTIMA<=19 & DPTO_DOMICILIO != "99") %>%
    #que han experimentado violencia psicologica/fisica/sexual
    filter(any(v_psi, v_fis, v_sex)) %>%
    mutate(
      v_pareja = VINCULO_PAREJA %in% c(1, 3, 5, 7),
      bullying =
        SIN_VINCULO %in% c(1, 3, 4, 6, 7) &
        abs(EDAD_VICTIMA - EDAD_AGRESOR) <= 3 &
        (v_psi|v_fis) & !v_sex,
      no_bullying =
        !(SIN_VINCULO %in% c(1, 3, 4, 6, 7) &
            abs(EDAD_VICTIMA - EDAD_AGRESOR) <= 3) &
        (v_psi|v_fis) & !v_sex,
      v_sexnp = v_sex & !(VINCULO_PAREJA %in% c(1, 3, 5, 7)),
      domestica =
        AGRESOR_VIVE_CASA_VICTIMA == 1 &
        (VINCULO_FAMILIAR|SIN_VINCULO == 5)
    ) %>%
    transmute(
      region = DPTO_DOMICILIO,
      area = AREA_RESIDENCIA_DOMICILIO,
      sex = SEXO_VICTIMA,
      v_pareja, bullying, no_bullying, v_sexnp, domestica
    ) %>%
    mutate(
      region = regiones[as.numeric(region)],
      area =
        case_when(
          area == "U" ~ "URBANA",
          area == "U" ~ "RURAL"
          ),
      sex =
        case_when(
          sex == 0 ~ "MUJER",
          sex == 1 ~ "HOMBRE"
        )
    )
) %>%
  evaluate_plan(
    rules = list(DATADIR__ = datadir),
    expand = FALSE
  )