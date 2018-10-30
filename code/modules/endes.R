out.endes <- function() {
  ### Cargar ENDES
  ## Household basic data
  rech0 <- import("endes/2017/Modulo64/RECH0.SAV" %>% paste0(ineidir,.), setclass = "data.table")
  ## Respondent basic data
  rec0111 <- import("endes/2017/Modulo66/REC0111.SAV" %>% paste0(ineidir,.), setclass = "data.table")
  ## Reproduction/Contraception
  rec21 <- import("endes/2017/Modulo67/REC21.SAV" %>% paste0(ineidir,.), setclass = "data.table")
  re223132 <- import("endes/2017/Modulo67/RE223132.SAV" %>% paste0(ineidir,.), setclass = "data.table")
  re516171 <- import("endes/2017/Modulo71/RE516171.SAV" %>% paste0(ineidir,.), setclass = "data.table")
  re758081 <- import("endes/2017/Modulo72/RE758081.SAV" %>% paste0(ineidir,.), setclass = "data.table")
  ## Cuestionario de Salud
  csalud01 <- data.table(haven::read_sav("endes/2017/Modulo414/CSALUD01.sav" %>% paste0(ineidir,.), encoding = "latin1"))
  ## Cuestionario de Salud (<12a)
  csalud08 <- data.table(haven::read_sav("endes/2017/Modulo414/CSALUD08.sav" %>% paste0(ineidir,.), encoding = "latin1"))
  ## Anemia mujer
  rec42 <- import("endes/2017/Modulo70/REC42.SAV" %>% paste0(ineidir,.), setclass = "data.table")
  ## Mortalidad materna
  rec83 <- import("endes/2017/Modulo73/REC83.SAV" %>% paste0(ineidir,.), setclass = "data.table")
  ## Violencia domestica
  rec84dv <- import("endes/2017/Modulo73/REC84DV.SAV" %>% paste0(ineidir,.), setclass = "data.table")
  
  # Unir bases
    # Encuestas solo mujer
  (rec0111 %>%
      merge(re223132, by = "CASEID", all.x = T) %>%
      merge(re516171, by = "CASEID", all.x = T) %>%
      merge(re758081, by = "CASEID", all.x = T) %>%
      merge(rec42, by = "CASEID", all.x = T) %>%
      merge(rec84dv, by = "CASEID", all.x = T))[,.(estrato.region=putlabel(V023), estrato.urbrur=V022, psuid=V021,
                                                   peso=V005/10**6,
                                                   gedad=putlabel(V013), inisex=V525,
                                                   nhijos=V209,
                                                   antic.moderno=as.numeric(V364==1),
                                                   antic.modotra=as.numeric(V364<=2),
                                                   ultsex.condon=case_when(V761 == 8 ~ NA_real_, TRUE ~ V761),
                                                   v.emoc=D104, v.fisi=D106, v.fisigrav=D107,v.sex=D108,
                                                   edad.matri=V511,
                                                   anemia=as.numeric(V457<4),
                                                   imc=ifelse(V445 == 9998, NA, V445))] %>%
    svydesign(ids=~psuid, strata=~estrato.region+estrato.urbrur, weights=~peso, data=.) -> dmujer
  
  # Encuesta salud, mujer y hombre
  (csalud01 %>%
      merge(rech0, by = "HHID", all.x = TRUE))[!is.na(PESO15_AMAS), #OJO! pesos faltantes estan siendo ignorados!
                                               .(estrato.region=putlabel(HV023), estrato.urbrur=HV022, psuid=HV021,
                                                 peso=PESO15_AMAS/10**6,
                                                 sexo=putlabel(QSSEXO), edad=QS23,
                                                 alc.vida=case_when(QS206 == 1 ~ 1, QS206 == 2 ~ 0, TRUE ~ NA_real_),
                                                 alc.anho=case_when(QS208 == 1 ~ 1, QS206 == 2 | QS208 == 2 ~ 0, TRUE ~ NA_real_),
                                                 alc.12va=case_when(QS209 == 1 ~ 1, QS206 == 2 | QS208 == 2 | QS209 == 2 ~ 0, TRUE ~ NA_real_),
                                                 alc.30d =case_when(QS210 == 1 ~ 1, QS206 == 2 | QS208 == 2 | QS209 == 2 | QS210 == 2 ~ 0, TRUE ~ NA_real_),
                                                 tabaco.anho=case_when(QS200 == 1 ~ 1, QS200 == 2 ~ 0, TRUE ~ NA_real_),
                                                 tabaco.30d =case_when(QS201 == 1 ~ 1, QS200 == 2 | QS201 == 2 ~ 0, TRUE ~ NA_real_),
                                                 v.golpe=as.numeric(QS710>1), v.arma=as.numeric(QS711>1))] %>%
    svydesign(ids=~psuid, strata=~estrato.region+estrato.urbrur, weights=~peso, data=.) -> dsalud
  
  list(
    dmujer %>% subset(as.numeric(gedad) == 1) %>% svymean( ~ nhijos, .),
    dmujer %>% subset(as.numeric(gedad) == 1) %>% svyciprop( ~ antic.moderno, .),
    dmujer %>% subset(as.numeric(gedad) == 1) %>% svyciprop( ~ antic.modotra, .),
    dmujer %>% subset(as.numeric(gedad) == 1 & inisex > 0) %>% svyciprop( ~ ultsex.condon, .),
    dmujer %>% subset(as.numeric(gedad) == 1) %>% svyciprop( ~ v.emoc, .),
    dmujer %>% subset(as.numeric(gedad) == 1) %>% svyciprop( ~ v.fisi, .),
    dmujer %>% subset(as.numeric(gedad) == 1) %>% svyciprop( ~ v.fisigrav, .),
    dmujer %>% subset(as.numeric(gedad) == 1) %>% svyciprop( ~ v.sex, .),
    dmujer %>% subset(as.numeric(gedad) == 1) %>% svyciprop( ~ I(v.emoc==1|v.sex==1|v.fisi==1|v.fisigrav==1), .),
    dmujer %>% subset(as.numeric(gedad) == 2) %>% svyciprop( ~ I(edad.matri < 15), .),
    dmujer %>% subset(as.numeric(gedad) == 2) %>% svyciprop( ~ I(edad.matri < 18), .),
    dmujer %>% subset(as.numeric(gedad) == 1) %>% svyciprop( ~ anemia, .),
    dmujer %>% subset(as.numeric(gedad) == 1) %>% svyciprop( ~ I(imc<1850|2500<imc), .),
    dsalud %>% subset(15<=edad&edad<=19) %>% svyciprop(~v.golpe, .),
    dsalud %>% subset(15<=edad&edad<=19) %>% svyciprop(~v.arma, .),
    dsalud %>% subset(15<=edad&edad<=19) %>% svyciprop(~I(v.arma==1|v.golpe==1), .),
    dsalud %>% subset(15<=edad&edad<=19) %>% svyciprop(~alc.vida, .),
    dsalud %>% subset(15<=edad&edad<=19) %>% svyciprop(~alc.anho, .),
    dsalud %>% subset(15<=edad&edad<=19) %>% svyciprop(~alc.12va, .),
    dsalud %>% subset(15<=edad&edad<=19) %>% svyciprop(~alc.30d , .),
    dsalud %>% subset(15<=edad&edad<=19) %>% svyciprop(~tabaco.anho, .),
    dsalud %>% subset(15<=edad&edad<=19) %>% svyciprop(~tabaco.30d , .)
  ) %>% lapply(svy2pci) %>% do.call(rbind, .) -> tmp.estimates
  
  colnames(tmp.estimates) <- c("valor", "CI95.Inf", "CI95.Sup")
  
  indnom <- c(
    "Nacidos vivos / 1000 mujeres 15-19",
    "% mujeres 15-19 usa anticonceptivo moderno",
    "% mujeres 15-19 usa anticonceptivo moderno o tradicional",
    "% mujeres 15-19 uso condon en ultimo encuentro sexual",
    "% mujeres 15-19 experimento violencia emocional",
    "% mujeres 15-19 experimento violencia fisica leve",
    "% mujeres 15-19 experimento violencia fisica grave",
    "% mujeres 15-19 experimento violencia sexual",
    "% mujeres 15-19 experimento cualquier violencia",
    "% mujeres 20-24 unidas antes de los 15",
    "% mujeres 20-24 unidas antes de los 18",
    "% mujeres 15-19 con anemia",
    "% mujeres 15-19 con bajo peso/sobre peso",
    "% personas 15-19 cuya pareja golpeo en ultimos 12 meses",
    "% personas 15-19 cuya pareja agredio con objeto en ultimos 12 meses",
    "% personas 15-19 cuya pareja agredio c/s objeto en ultimos 12 meses",
    "% personas 15-19 han consumido alcohol en su vida",
    "% personas 15-19 han consumido alcohol en ultimos 12 meses",
    "% personas 15-19 han consumido alcohol >=12 veces en ultimos 12 meses",
    "% personas 15-19 han consumido alcohol en ultimos 30 dias",
    "% personas 15-19 han consumido tabaco en ultimos 12 meses",
    "% personas 15-19 han consumido tabaco en ultimos 30 dias"
  )
  
  "# Indicadores ENDES
  #> Porcentaje de adolescentes que han experimentado violencia física, sexual o emocional por parte de una pareja
    ## D005, peso de DV no esta! Preguntar a Mixsi Casas kefue
  # X (pendiente revisar) Tasa de denuncia de violencia doméstica
    ## Pide ayuda a policia en D119XE pero hay otras opciones, conversarlo
    ## ... en realidad esta casi todo vacio: rec84dv[,160:170] %>% apply(2,table)
    ## solo hay datos en las preguntas relacionadas a familiares" -> comments
  
  cbind(indnom,
        data.frame(round(tmp.estimates*c(1000,rep(100,nrow(tmp.estimates)-1)),2)),
        fuente = "ENDES 2017") %>% list(content=., comments=comments)
}

out.endes <- out.endes()

# Para despues:
#svyby(~nhijos, ~estrato.region, design = dsalud.natalidad, svymean, na.rm = T)
#svyby(~nhijos, ~estrato.region, design = subset(dsalud.natalidad, !is.na(nhijos)), svymean)