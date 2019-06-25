plan_dimensions <- drake_plan(
  tabla_interdimensional =
    rbind(
      rbind(
        # SOLO MUJER / MAX REGION
        tasa_natalidad %>%
          tabfun("Tasa de natalidad en mujeres 15-19 años",
                 "ENDES 2017", scale = 1000),
        prevalencia_mala.nutricion %>%
          tabfun("% mujeres 15-19 años con desnutrición o sobrepeso",
                 "ENDES 2017"),
        prevalencia_anemia %>%
          tabfun("% mujeres 15-19 años con anemia",
                 "ENDES 2017"),
        prevalencia_tab.alc30d %>%
          tabfun("% adolescentes 15-19 años que consumió alcohol o tabaco en los últimos 30 días",
                 "ENDES 2017"),
        prevalencia_condon %>%
          tabfun("% mujeres 15-19 años que usó condón en último encuentro sexual",
                 "ENDES 2017", highbad = F),
        prevalencia_metodo.moderno %>%
          tabfun("% mujeres 15-19 años sexualmente activas que usan método anticonceptivo moderno",
                 "ENDES 2017", highbad = F),
        minsa_depresion %>%
          tabfun("Adolescentes 12-17 años atendidos por depresión x 1000 habitantes", "HIS 2017", scale = 1),
        minsa_defunciones %>%
          tabfun("Tasa de mortalidad global en adolescentes 12-17 años", "MINSA 2016", scale = 1)
        ) %>% cbind(dimension = "SALUD"),
      rbind(
        finalizacion_primaria %>%
          tabfun("Tasa de finalización oportuna de educación primaria",
                 "ENAHO 2017", highbad = F),
        finalizacion_secundaria %>%
          tabfun("Tasa de finalización oportuna de educación secundaria",
                 "ENAHO 2017", highbad = F),
        out_of_school %>%
          tabfun("% adolescentes 12-17 años que no estudia en el periodo",
                 "ENAHO 2017"),
        matricula_superior %>%
          tabfun("% jóvenes 18-22 años con educación superior completa o en proceso",
                 "ENAHO 2017", highbad = F),
        competencia_lect %>%
          tabfun("% estudiantes de 2do de secundaria con competencia satisfactoria en lectura",
                 "ECE 2016", scale = 1, highbad = F),
        competencia_mate %>%
          tabfun("% estudiantes de 2do de secundaria con competencia satisfactoria en matemática",
                 "ECE 2016", scale = 1, highbad = F),
        aspira_superior %>%
          tabfun("% adolescentes de 15 años que aspira a educación superior",
                 "PISA 2015", scale = 1, highbad = F)
        ) %>% cbind(dimension = "EDUCACION"),
      rbind(
        matrimonio_infantil %>%
          tabfun("% mujeres 20-24 años casadas antes de los 18 años",
                 "ENDES 2017"),
        mujer_violentada %>%
          tabfun("% mujeres 15-19 años que han experimentado violencia por parte de su pareja en algún momento de su vida",
                 "ENDES 2017"),
        violencia_no.bulli %>%
          tabfun("% escolares 12-17 años que han experimentado violencia en el hogar en algún momento de su vida",
                 "ENARES 2015"),
        violencia_bullying %>%
          tabfun("% escolares 12-17 años que han experimentado bullying en algún momento de su vida",
                 "ENARES 2015"),
        violencia_sexual %>%
          tabfun("% escolares 12-17 años que han sido víctimas de violencia sexual por parte de alguien que no es su pareja en algún momento de su vida",
                 "ENARES 2015"),
        # pobreza_monetaria %>%
        #   tabfun("Proporción de adolescentes entre 15-19 en pobreza monetaria",
        #          "ENAHO 2017"),
        vida_satisfac %>%
          tabfun("% de adolescentes de 15 años que indica estar completamente satisfecho con su vida",
                 "PISA 2015", scale = 1, highbad = F),
        cem_denuncias %>%
          tabfun("Casos de violencia doméstica reportados a un CEM en adolescentes 12-17 años x 1000 habitantes",
                 "CEM 2017", scale = 1)
        ) %>% cbind(dimension = "SEGURIDAD"),
      rbind(
        trabajo_infantil_enaho %>%
          tabfun("% adolescentes 12-17 años que exceden umbral OIT de horas trabajadas por semana",
                 "ENAHO 2017"),
        porcentaje_nini %>%
          tabfun("% adolescentes 15-19 años sin educación, empleo o formación (nini)",
                 "ENAHO 2017"),
        conoce_financiero %>%
          tabfun("% adolescentes de 15 años con conocimiento financiero mínimamente satisfactorio o mejor",
                 "PISA 2015", scale = 1, highbad = F)
        ) %>% cbind(dimension = "TRABAJO"),
      rbind(
        # participacion_sindicato %>%
        #   tabfun("Participación de adolescentes en sindicatos",
        #          "ENUT 2010", highbad = F),
        tiempo_recreativo %>%
          tabfun("% adolescentes 12-17 años que tuvo al menos 24 horas de actividades recreativas en la última semana",
                 "ENUT 2010", highbad = F),
        # voluntariado %>%
        #   tabfun("Indicador de voluntariado",
        #          "ENUT 2010", highbad = F),
        uso_internet %>%
          tabfun("% adolescentes 12-17 años que usó Internet en el último mes",
                 "ENAHO 2017", highbad = F),
        confianza_gob %>%
          tabfun("% adolescentes 12-17 años que indica que confía mucho o plenamente en el gobierno nacional",
                 "ICCS 2016", highbad = F),
        opinion_cole %>%
          tabfun("% adolescentes 12-17 años que manifiesta que sus profesores los motivan con frecuencia a expresar sus opiniones",
                 "ICCS 2016", highbad = F)
        # jovenes_votan %>%
        #   tabfun("% de jovenes 18-24 habilitados para votar",
        #          "JNE 2017", highbad = F, scale = 1)
        ) %>% cbind(dimension = "PARTICIPACION")
    ) %>%
    mutate(desag =
             desag %>%
             toupper %>%
             iconv(from = "latin1",
                   to = "ASCII//TRANSLIT") %>%
             ubigeator),
  
  tabla_limites =
    (function(){
      tabla_interdimensional %>%
      filter(desag %in% regiones) %>%
      group_by(nombre) %>%
      summarize(minval = min(ind, na.rm = T),
                maxval = max(ind, na.rm = T)) -> tmp
      tibble(nombre =
               tabla_interdimensional$nombre %>%
               unique %>%
               setdiff(tmp$nombre),
             minval = 0,
             maxval = 100) -> sinregion
      bind_rows(tmp, sinregion)
      })(),
  
  tabla_normalizada =
    tabla_interdimensional %>%
    left_join(tabla_limites, by = "nombre") %>%
    mutate(upper = ifelse(highbad, minval, maxval),
           lower = ifelse(highbad, maxval, minval)) %>%
    mutate(norm = ind %>% normind(upper, lower)) %>%
    transmute(
      desag,
      dimension,
      nombre,
      ind,
      error,
      peor = lower,
      mejor = upper,
      norm,
      fuente)
)