out.enares <- function() {
enares200 <- import("enares/503-Modulo770/14_CRS04_CAP200.sav" %>% paste0(ineidir,.), setclass = "data.table")

enares200[,.(peso = Factor_Alumnos, estrato = putlabel(AREA), psu = as.numeric(factor(COD_MOD)),
             sexo = putlabel(SEXO), edad = EDAD,
             v.emo = as.numeric(
               mapply(function(...)2*length(list(...))-sum(...),
                      C4P225_1,C4P225_2,C4P225_3,C4P225_4,C4P225_5,
                      C4P225_6,C4P225_7,C4P225_8,C4P225_9,C4P225_10,
                      C4P225_11,C4P225_12,C4P225_13,C4P225_14)>0),
             v.fis = as.numeric(
               mapply(function(...)2*length(list(...))-sum(...),
                      C4P229_1,C4P229_2,C4P229_3,C4P229_4,C4P229_5,
                      C4P229_6,C4P229_7,C4P229_8,C4P229_9,C4P229_10)>0),
             v.sex = as.numeric(
               mapply(function(...)2*length(list(...))-sum(...),
                      C4P248_1,C4P248_2,C4P248_3,C4P248_4,C4P248_5,
                      C4P248_6,C4P248_7,C4P248_8,C4P248_9,C4P248_10,
                      C4P248_11,C4P248_12)>0)
          )] %>%
  svydesign(ids=~psu, strata=~estrato, weights=~peso, data=.) -> denares

# # Porcentaje de adolescentes que han experimentado violencia (12-17) (las preg de emocional y fisica se restringen a colegio)
# denares %>% svyciprop(~I((v.emo+v.fis+v.sex)>0), .)
# #   Violencia sexual ejercida por otra persona que no es su pareja (12-17)
# denares %>% svyciprop(~v.sex, .)
# #   Violencia de cualquier tipo en colegio (emocional/fisica = bullying) (12-17) ... incluir sexual?
# denares %>% svyciprop(~I((v.emo+v.fis)>0), .)

list(denares %>% svyciprop(~I((v.emo+v.fis+v.sex)>0), .),
     denares %>% svyciprop(~v.sex, .),
     denares %>% svyciprop(~I((v.emo+v.fis)>0), .)) %>%  lapply(svy2pci) %>% do.call(rbind, .) -> tmp.estimates

colnames(tmp.estimates) <- c("valor", "CI95.Inf", "CI95.Sup")
  
indnom <- c(
  "% de adolescentes que han experimentado violencia (cualquiera: emocional, fisica, sexual)",
  "% de adolescentes que han experimentado violencia sexual ejercida por otra persona que no es su pareja (no se puede excluir pareja - ver comentarios)",
  "% de adolescentes que reportaron ser victimas de bullying (preguntas de violencia emocional o fisica)"
)
  
"Los tres indicadores miden casi lo mismo. En v.sexual no se puede excluir pareja por falta de informacion en 'otros' y algunos casos podrian no ser violencia bajo esa situacion" -> comments
  
cbind(indnom,
      data.frame(round(tmp.estimates*c(rep(100,nrow(tmp.estimates))),2)),
      fuente = "ENARES 2015") %>% list(content=., comments=comments)
}

out.enares <- out.enares()

######### Falta considerar violencia en albergue/hogar