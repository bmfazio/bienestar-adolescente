plan_dimensions <- drake_plan(
  tabla_interdimensional =
    rbind(
      rbind(
        # SOLO MUJER / MAX REGION
        tasa_natalidad %>%
          tabfun("Tasa de natalidad adolescente", 1000),
        prevalencia_mala.nutricion %>%
          tabfun("Prevalencia de desnutrición o sobrepeso"),
        prevalencia_anemia %>%
          tabfun("Prevalencia de anemia"),
        prevalencia_tab.alc30d %>%
          tabfun("Prevalencia de consumo reciente de alcohol o tabaco"),
        prevalencia_condon %>%
          tabfun("Proporción de adolescentes que usó condón en último encuentro sexual"),
        prevalencia_metodo.moderno %>%
          tabfun("Proporción de adolescentes sexualmente activos con método moderno")
        ) %>% cbind(dimension = "SALUD"),
      rbind(
        finalizacion_primaria %>%
          tabfun("Tasa de finalización de educación primaria"),
        finalizacion_secundaria %>%
          tabfun("Tasa de finalización de educación secundaria"),
        out_of_school %>%
          tabfun("Tasa de adolescentes fuera del sistema educativo"),
        matricula_superior %>%
          tabfun("Tasa de matrícula bruta en educación superior"),
        competencia_lect %>%
          tabfun("Nivel de competencia en lectura", 1),
        competencia_mate %>%
          tabfun("Nivel de competencia en matemática", 1),
        aspira_superior %>%
          tabfun("% que aspira a educación superior", 1)
        ) %>% cbind(dimension = "EDUCACION"),
      rbind(
        matrimonio_infantil %>%
          tabfun("Menores de edad unidas"),
        mujer_violentada %>%
          tabfun("% adolescentes que han experimentado violencia por parte de su pareja"),
        violencia_no.bulli %>%
          tabfun("% adolescentes que han experimentado violencia (no bullying)"),
        violencia_bullying %>%
          tabfun("% adolescentes que reportaron ser víctimas de bullying"),
        violencia_sexual %>%
          tabfun("Violencia sexual ejercida por otra persona que no es su pareja"),
        pobreza_monetaria %>%
          tabfun("Proporción de adolescentes entre 15-19 en pobreza monetaria"),
        vida_satisfac %>%
          tabfun("% completamenta satisfecho con su vida"),
        pnp_denuncias %>%
          tabfun("Denuncias por violencia doméstica x 1000 habitantes", 1)
        ) %>% cbind(dimension = "SEGURIDAD"),
      rbind(
        tinfantil_full %>%
          tabfun("% adolescentes involucrados en trabajo infantil (c/tiempo hogar)"),
        porcentaje_nini %>%
          tabfun("% adolescentes sin educación, empleo o formación (nini)"),
        conoce_financiero %>%
          tabfun("% con conocimiento financiero básico o mejor", 1)
        ) %>% cbind(dimension = "TRABAJO"),
      rbind(
        participacion_sindicato %>%
          tabfun("Participación de adolescentes en sindicatos"),
        tiempo_recreativo %>%
          tabfun("% adolescentes en actividades recreacionales por un periodo específico", 1),
        voluntariado %>%
          tabfun("Indicador de voluntariado"),
        uso_internet %>%
          tabfun("% adolescentes que usaron Internet en el último mes"),
        confianza_gob %>%
          tabfun("% que confía mucho o plenamente en el gobierno nacional"),
        opinion_cole %>%
          tabfun("% que manifiesta que sus profesores los motivan con frecuencia a expresar sus opiniones")
        ) %>% cbind(dimension = "PARTICIPACION")
    ) %>%
    mutate(desag =
             desag %>%
             toupper %>%
             iconv(from = "UTF-8",
                   to = "ASCII//TRANSLIT") %>%
             ubigeator),
  
  tabla_limites = rbind.data.frame(
    list(nombre = "Tasa de natalidad adolescente",
         fuente = "ENDES 2017",
         lower = 0, upper = 100),
    list(nombre = "Prevalencia de desnutrición o sobrepeso",
         fuente = "ENDES 2017",
         lower = 0, upper = 100),
    list(nombre = "Prevalencia de anemia",
         fuente = "ENDES 2017",
         lower = 0, upper = 100),
    list(nombre = "Prevalencia de consumo reciente de alcohol",
         fuente = "ENDES 2017",
         lower = 0, upper = 100),
    list(nombre = "Prevalencia de consumo reciente de tabaco",
         fuente = "ENDES 2017",
         lower = 0, upper = 100),
    list(nombre = "Prevalencia de consumo reciente de alcohol o tabaco",
         fuente = "ENDES 2017",
         lower = 0, upper = 100),
    list(nombre = "Proporción de adolescentes que usó condón en último encuentro sexual",
         fuente = "ENDES 2017",
         lower = 100, upper = 0),
    list(nombre = "Proporción de adolescentes sexualmente activos con método moderno",
         fuente = "ENDES 2017",
         lower = 100, upper = 0),
    list(nombre = "Tasa de finalización de educación primaria",
         fuente = "ENAHO 2017",
         lower = 100, upper = 0),
    list(nombre = "Tasa de finalización de educación secundaria",
         fuente = "ENAHO 2017",
         lower = 100, upper = 0),
    list(nombre = "Tasa de adolescentes fuera del sistema educativo",
         fuente = "ENAHO 2017",
         lower = 0, upper = 100),
    list(nombre = "Tasa de matrícula bruta en educación superior",
         fuente = "ENAHO 2017",
         lower = 100, upper = 0),
    list(nombre = "Nivel de competencia lectura/matematicas",
         fuente = "ECE 2016",
         lower = 100, upper = 0),
    list(nombre = "Nivel de competencia en lectura",
         fuente = "ECE 2016",
         lower = 100, upper = 0),
    list(nombre = "Nivel de competencia en matemática",
         fuente = "ECE 2016",
         lower = 100, upper = 0),
    list(nombre = "Menores de edad unidas",
         fuente = "ENDES 2017",
         lower = 0, upper = 100),
    list(nombre = "% adolescentes que han experimentado violencia por parte de su pareja",
         fuente = "ENDES 2017",
         lower = 0, upper = 100),
    list(nombre = "% adolescentes que han experimentado violencia",
         fuente = "ENARES 2015",
         lower = 0, upper = 100),
    list(nombre = "% adolescentes que han experimentado violencia (no bullying)",
         fuente = "ENARES 2015",
         lower = 0, upper = 100),
    list(nombre = "% adolescentes que reportaron ser víctimas de bullying",
         fuente = "ENARES 2015",
         lower = 0, upper = 100),
    list(nombre = "Violencia sexual ejercida por otra persona que no es su pareja",
         fuente = "ENARES 2015",
         lower = 0, upper = 100),
    list(nombre = "Proporción de adolescentes entre 15-19 en pobreza monetaria",
         fuente = "ENAHO 2017",
         lower = 0, upper = 100),
    list(nombre = "Tiempo dedicado a tareas del hogar, no remunerado",
         fuente = "ETI 2015",
         lower = 0, upper = 36),
    list(nombre = "% adolescentes involucrados en trabajo infantil (s/tiempo hogar)",
         fuente = "ENAHO 2017",
         lower = 0, upper = 100),
    list(nombre = "% adolescentes involucrados en trabajo infantil (c/tiempo hogar)",
         fuente = "ENAHO 2017",
         lower = 0, upper = 100),
    list(nombre = "Tasa de desempleo adolescente",
         fuente = "ENAHO 2017",
         lower = 0, upper = 100),
    list(nombre = "% adolescentes sin educación, empleo o formación (nini)",
         fuente = "ENAHO 2017",
         lower = 0, upper = 100),
    list(nombre = "% adolescentes enrolados en programas de capacitacion tecnico vocacional",
         fuente = "ENUT 2010",
         lower = 0, upper = 100),
    list(nombre = "Participación de adolescentes en sindicatos",
         fuente = "ENUT 2010",
         lower = 100, upper = 0),
    list(nombre = "% adolescentes en actividades recreacionales por un periodo específico",
         fuente = "ENUT 2010",
         lower = 100, upper = 0),
    list(nombre = "Indicador de voluntariado",
         fuente = "ENUT 2010",
         lower = 100, upper = 0),
    list(nombre = "% adolescentes que usaron Internet en el último mes",
         fuente = "ENAHO 2017",
         lower = 100, upper = 0)
    ),
  
  tabla_normalizada =
    tabla_interdimensional %>%
    left_join(tabla_limites, by = "nombre") %>%
    mutate(norm = ind %>% normind(lower, upper)) %>%
    select(
      desag,
      dimension,
      nombre,
      ind,
      error,
      lower,
      upper,
      norm,
      fuente)
)