eti_load <- drake_plan(
  eti.ninhos = 
    import(
      file_in("INEIDIR__/eti/Modulo1036/ETI_2015_Ninos_5_a_17anios.sav"),
      setclass = "data.table"),
  eti_ready =
    eti.ninhos[,.(
      psu = QHCONGLOMERADO,
      peso = FACTOREXP_NIÑOS,
      estrato1 = QHDEPARTAMENTO,
      estrato2 = ESTRATO,
      sexo = putlabel(QC_SEXO),
      edad = QC_EDAD,
      tdomesticoLV = as.numeric(
        mapply(function(...)2*length(list(...))-sum(...),
               QD301A_1 %>% ifelse(.==9,NA,.),
               QD301A_2 %>% ifelse(.==9,NA,.),
               QD301A_3 %>% ifelse(.==9,NA,.),
               QD301A_4 %>% ifelse(.==9,NA,.),
               QD301A_5 %>% ifelse(.==9,NA,.),
               QD301A_6 %>% ifelse(.==9,NA,.),
               QD301A_7 %>% ifelse(.==9,NA,.))>0),
      tdomesticoLV.tiempo =
        (QD301B %>% ifelse(.==9,NA,.))*
        ((QD301C_H %>% ifelse(.==99,NA,.))+(QD301C_M %>% ifelse(.==99,NA,.))/60),
      tdomesticoSD = as.numeric(
        mapply(function(...)2*length(list(...))-sum(...),
               QD302A_1 %>% ifelse(.==9,NA,.),
               QD302A_2 %>% ifelse(.==9,NA,.),
               QD302A_3 %>% ifelse(.==9,NA,.),
               QD302A_4 %>% ifelse(.==9,NA,.),
               QD302A_5 %>% ifelse(.==9,NA,.),
               QD302A_6 %>% ifelse(.==9,NA,.),
               QD302A_7 %>% ifelse(.==9,NA,.))>0),
      tdomesticoSD.tiempo = (
        QD302B %>% ifelse(.==9,NA,.))*
        ((QD302C_H %>% ifelse(.==99,NA,.))+(QD302C_M %>% ifelse(.==99,NA,.))/60),
      trabtiempo=
        ((QD418A_DOM_MHH-QD418A_DOM_MDH)+(QD418A_DOM_MHM-QD418A_DOM_MDM)/60)+
        ((QD418A_DOM_THH-QD418A_DOM_TDH)+(QD418A_DOM_THM-QD418A_DOM_TDM)/60)+
        ((QD418A_DOM_NHH-QD418A_DOM_NDH)+(QD418A_DOM_NHM-QD418A_DOM_NDM)/60)+
        ((QD418A_LUN_MHH-QD418A_LUN_MDH)+(QD418A_LUN_MHM-QD418A_LUN_MDM)/60)+
        ((QD418A_LUN_THH-QD418A_LUN_TDH)+(QD418A_LUN_THM-QD418A_LUN_TDM)/60)+
        ((QD418A_LUN_NHH-QD418A_LUN_NDH)+(QD418A_LUN_NHM-QD418A_LUN_NDM)/60)+
        ((QD418A_MAR_MHH-QD418A_MAR_MDH)+(QD418A_MAR_MHM-QD418A_MAR_MDM)/60)+
        ((QD418A_MAR_THH-QD418A_MAR_TDH)+(QD418A_MAR_THM-QD418A_MAR_TDM)/60)+
        ((QD418A_MAR_NHH-QD418A_MAR_NDH)+(QD418A_MAR_NHM-QD418A_MAR_NDM)/60)+
        ((QD418A_MIE_MHH-QD418A_MIE_MDH)+(QD418A_MIE_MHM-QD418A_MIE_MDM)/60)+
        ((QD418A_MIE_THH-QD418A_MIE_TDH)+(QD418A_MIE_THM-QD418A_MIE_TDM)/60)+
        ((QD418A_MIE_NHH-QD418A_MIE_NDH)+(QD418A_MIE_NHM-QD418A_MIE_NDM)/60)+
        ((QD418A_JUE_MHH-QD418A_JUE_MDH)+(QD418A_JUE_MHM-QD418A_JUE_MDM)/60)+
        ((QD418A_JUE_THH-QD418A_JUE_TDH)+(QD418A_JUE_THM-QD418A_JUE_TDM)/60)+
        ((QD418A_JUE_NHH-QD418A_JUE_NDH)+(QD418A_JUE_NHM-QD418A_JUE_NDM)/60)+
        ((QD418A_VIE_MHH-QD418A_VIE_MDH)+(QD418A_VIE_MHM-QD418A_VIE_MDM)/60)+
        ((QD418A_VIE_THH-QD418A_VIE_TDH)+(QD418A_VIE_THM-QD418A_VIE_TDM)/60)+
        ((QD418A_VIE_NHH-QD418A_VIE_NDH)+(QD418A_VIE_NHM-QD418A_VIE_NDM)/60)+
        ((QD418A_SAB_MHH-QD418A_SAB_MDH)+(QD418A_SAB_MHM-QD418A_SAB_MDM)/60)+
        ((QD418A_SAB_THH-QD418A_SAB_TDH)+(QD418A_SAB_THM-QD418A_SAB_TDM)/60)+
        ((QD418A_SAB_NHH-QD418A_SAB_NDH)+(QD418A_SAB_NHM-QD418A_SAB_NDM)/60)+
        (QD418B%>%ifelse(.==999,0,.))
      )] %>%
    mutate(
      tdomesticoLV.tiempo =
        ifelse(tdomesticoLV == 0, 0, tdomesticoLV.tiempo),
      tdomesticoSD.tiempo =
        ifelse(tdomesticoSD == 0, 0, tdomesticoSD.tiempo)
      ) %>%
    svydesign(ids =~ psu,
              strata =~ estrato1 + estrato2,
              weights =~ peso,
              data = .)
)  %>%
  evaluate_plan(
    rules = list(INEIDIR__ = ineidir),
    expand = FALSE
  )