plan_dimensions <- drake_plan(
  tabla_salud = cbind(
    data.frame(
      indicator = c(
        "Tasa de natalidad adolescente",
        "Prevalencia de desnutrición o sobrepeso",
        "Prevalencia de anemia",
        "Prevalencia de consumo reciente de alcohol",
        "Prevalencia de consumo reciente de tabaco",
        "Proporción de adolescentes que usó condón en último encuentro sexual",
        "Proporción de adolescentes sexualmente activos con método moderno"
        )),
    as.data.frame(matrix(c(
      svy2pci(tasa_natalidad)*1000,
      svy2pci(prevalencia_mala.nutricion)*100,
      svy2pci(prevalencia_anemia)*100,
      svy2pci(prevalencia_alcohol30d)*100,
      svy2pci(prevalencia_tabaco30d)*100,
      svy2pci(prevalencia_condon)*100,
      svy2pci(prevalencia_metodo.moderno)*100
      ),
      ncol = 3,
      byrow = TRUE,
      dimnames = list(NULL, c("value", "lower95", "upper95"))
    )),
    good = c(
      0, 0, 0, 0, 0, 100, 100),
    bad = c(
      100, 100, 100, 100, 100, 0, 0)
    ) %>%
    mutate(
      normalized = value %>% normind(max = good, min = bad)
      ),
  
  tabla_educacion = cbind(
    data.frame(
      indicator = c(
        "Tasa de finalización de educación primaria",
        "Tasa de finalización de educación secundaria",
        "Tasa de adolescentes fuera del sistema educativo",
        "Tasa de matrícula bruta en educación superior",
        "Nivel de competencia lectura/matematicas"
        )),
    as.data.frame(matrix(c(
      svy2pci(finalizacion_primaria)*100,
      svy2pci(finalizacion_secundaria)*100,
      svy2pci(out_of_school)*100,
      svy2pci(matricula_superior)*100,
      svy2pci(competencia_lect.mate)
      ),
      ncol = 3,
      byrow = TRUE,
      dimnames = list(NULL, c("value", "lower95", "upper95"))
    )),
    good = c(
      100, 100, 0, 100, 100),
    bad = c(
      0, 0, 100, 0, 0)
    ) %>%
    mutate(
      normalized = value %>% normind(max = good, min = bad)
      ),
  
  tabla_seguridad = cbind(
    data.frame(
      indicator = c(
        "Menores de edad unidas",
        "% adolescentes que han experimentado violencia por parte de su pareja",
        "% adolescentes que han experimentado violencia",
        "% adolescentes que reportaron ser víctimas de bullying",
        "Violencia sexual ejercida por otra persona que no es su pareja",
        "Proporción de adolescentes entre 15-19 en pobreza monetaria*"
        )),
    as.data.frame(matrix(c(
      svy2pci(matrimonio_infantil)*100,
      svy2pci(mujer_violentada)*100,
      svy2pci(violencia_cualquier)*100,
      svy2pci(violencia_bullying)*100,
      svy2pci(violencia_sexual)*100,
      svy2pci(pobreza_monetaria)*100
      ),
      ncol = 3,
      byrow = TRUE,
      dimnames = list(NULL, c("value", "lower95", "upper95"))
    )),
    good = rep(0, 6),
    bad = rep(100, 6)
    ) %>%
    mutate(
      normalized = value %>% normind(max = good, min = bad)
      ),

  tabla_trabajo = cbind(
    data.frame(
      indicator = c(
        "Tiempo dedicado a tareas del hogar, no remunerado",
        "% adolescentes involucrados en trabajo infantil (s/tiempo hogar)",
        "% adolescentes involucrados en trabajo infantil (c/tiempo hogar)",
        "Tasa de desempleo adolescente",
        "% adolescentes sin educación, empleo o formación (nini)",
        "% adolescentes enrolados en programas de capacitacion tecnico vocacional"
        )),
    as.data.frame(matrix(c(
      svy2pci(horas_trabajo.hogar),
      svy2pci(tinfantil_shogar)*100,
      svy2pci(tinfantil_full)*100,
      svy2pci(desempleo_adolescente)*100,
      svy2pci(porcentaje_nini)*100,
      svy2pci(enrolamiento_cetpro)*100
      ),
      ncol = 3,
      byrow = TRUE,
      dimnames = list(NULL, c("value", "lower95", "upper95"))
    )),
    good = rep(0, 6),
    bad = rep(100, 6)
    ) %>%
    mutate(
      normalized = value %>% normind(max = good, min = bad)
      ),

  tabla_participacion = cbind(
    data.frame(
      indicator = c(
        "Participación de adolescentes en sindicatos",
        "% adolescentes en actividades recreacionales/sociales por un periodo específico durante la semana",
        "Indicador de voluntariado",
        "% adolescentes que usaron Internet en el último mes"
        )),
    as.data.frame(matrix(c(
      svy2pci(participacion_sindicato)*100,
      svy2pci(tiempo_recreativo),
      svy2pci(voluntariado)*100,
      svy2pci(uso_internet)*100
      ),
      ncol = 3,
      byrow = TRUE,
      dimnames = list(NULL, c("value", "lower95", "upper95"))
    )),
    good = rep(100, 4),
    bad = rep(0, 4)
    ) %>%
    mutate(
      normalized = value %>% normind(max = good, min = bad)
      )
)

# # Para despues:
# #svyby(~nhijos, ~estrato.region, design = dsalud.natalidad, svymean, na.rm = T)
# #svyby(~nhijos, ~estrato.region, design = subset(dsalud.natalidad, !is.na(nhijos)), svymean)
