plan_dimensions <- drake_plan(
  tabla_interdimensional =
    rbind(
      rbind(
        # SOLO MUJER / MAX REGION
        tasa_natalidad %>%
          tabfun("Tasa de natalidad adolescente",
                 "ENDES 2017", scale = 1000),
        prevalencia_mala.nutricion %>%
          tabfun("Prevalencia de desnutrición o sobrepeso",
                 "ENDES 2017"),
        prevalencia_anemia %>%
          tabfun("Prevalencia de anemia",
                 "ENDES 2017"),
        prevalencia_tab.alc30d %>%
          tabfun("Prevalencia de consumo reciente de alcohol o tabaco",
                 "ENDES 2017"),
        prevalencia_condon %>%
          tabfun("Proporción de adolescentes que usó condón en último encuentro sexual",
                 "ENDES 2017", highbad = F),
        prevalencia_metodo.moderno %>%
          tabfun("Proporción de adolescentes sexualmente activos con método moderno",
                 "ENDES 2017", highbad = F),
        minsa_depresion %>%
          tabfun("Prevalencia de depresión", "HIS 2017", scale = 1),
        minsa_defunciones %>%
          tabfun("Tasa de mortalidad global", "MINSA 2016", scale = 1)
        ) %>% cbind(dimension = "SALUD"),
      rbind(
        finalizacion_primaria %>%
          tabfun("Tasa de finalización de educación primaria",
                 "ENAHO 2017", highbad = F),
        finalizacion_secundaria %>%
          tabfun("Tasa de finalización de educación secundaria",
                 "ENAHO 2017", highbad = F),
        out_of_school %>%
          tabfun("Tasa de adolescentes fuera del sistema educativo",
                 "ENAHO 2017"),
        matricula_superior %>%
          tabfun("Tasa de matrícula bruta en educación superior",
                 "ENAHO 2017", highbad = F),
        competencia_lect %>%
          tabfun("% con competencia satisfactoria en lectura",
                 "ECE 2016", scale = 1, highbad = F),
        competencia_mate %>%
          tabfun("% con competencia satisfactoria en matemática",
                 "ECE 2016", scale = 1, highbad = F),
        aspira_superior %>%
          tabfun("% que aspira a educación superior",
                 "PISA 2015", scale = 1, highbad = F)
        ) %>% cbind(dimension = "EDUCACION"),
      rbind(
        matrimonio_infantil %>%
          tabfun("Menores de edad unidas",
                 "ENDES 2017"),
        mujer_violentada %>%
          tabfun("% adolescentes que han experimentado violencia por parte de su pareja",
                 "ENDES 2017"),
        violencia_no.bulli %>%
          tabfun("% adolescentes que han experimentado violencia (no bullying)",
                 "ENARES 2015"),
        violencia_bullying %>%
          tabfun("% adolescentes que reportaron ser víctimas de bullying",
                 "ENARES 2015"),
        violencia_sexual %>%
          tabfun("Violencia sexual ejercida por otra persona que no es su pareja",
                 "ENARES 2015"),
        pobreza_monetaria %>%
          tabfun("Proporción de adolescentes entre 15-19 en pobreza monetaria",
                 "ENAHO 2017"),
        vida_satisfac %>%
          tabfun("% completamente satisfecho con su vida",
                 "PISA 2015", scale = 1, highbad = F),
        pnp_denuncias %>%
          tabfun("Denuncias por violencia doméstica x 1000 habitantes",
                 "PNP 2017", scale = 1)
        ) %>% cbind(dimension = "SEGURIDAD"),
      rbind(
        trabajo_infantil_enaho %>%
          tabfun("% adolescentes involucrados en trabajo infantil (c/tiempo hogar)",
                 "ENAHO 2017"),
        porcentaje_nini %>%
          tabfun("% adolescentes sin educación, empleo o formación (nini)",
                 "ENAHO 2017"),
        conoce_financiero %>%
          tabfun("% con conocimiento financiero suficiente",
                 "PISA 2015", scale = 1, highbad = F)
        ) %>% cbind(dimension = "TRABAJO"),
      rbind(
        # participacion_sindicato %>%
        #   tabfun("Participación de adolescentes en sindicatos",
        #          "ENUT 2010", highbad = F),
        tiempo_recreativo %>%
          tabfun("% adolescentes en actividades recreacionales por un periodo específico",
                 "ENUT 2010", highbad = F),
        # voluntariado %>%
        #   tabfun("Indicador de voluntariado",
        #          "ENUT 2010", highbad = F),
        uso_internet %>%
          tabfun("% adolescentes que usaron Internet en el último mes",
                 "ENAHO 2017", highbad = F),
        confianza_gob %>%
          tabfun("% que confía mucho o plenamente en el gobierno nacional",
                 "ICCS 2016", highbad = F),
        opinion_cole %>%
          tabfun("% que manifiesta que sus profesores los motivan con frecuencia a expresar sus opiniones",
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