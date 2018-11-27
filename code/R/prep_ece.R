ece_load <- drake_plan(
  ece_notas_read = read_xlsx(
    file_in("MINEDUDIR__/ece/EvaluacionCensal_Secundaria_SEGUNDO_14112018_160622.xlsx"), range = "J9:J278"),
  ece_lugar_read = read_xlsx(
    file_in("D:/datasets/minedu/ece/EvaluacionCensal_Secundaria_SEGUNDO_14112018_160622.xlsx"), range = "E9:E278"),
  ece_notas_math = read_xlsx(
    file_in("D:/datasets/minedu/ece/EvaluacionCensal_Secundaria_SEGUNDO_14112018_160622.xlsx"), range = "J286:J555"),
  ece_lugar_math = read_xlsx(
    file_in("D:/datasets/minedu/ece/EvaluacionCensal_Secundaria_SEGUNDO_14112018_160622.xlsx"), range = "E286:E555"),
  ece_ready =
    cbind(
      region =
        c(
          "NACIONAL",
          ece_lugar_read[c(which(is.na(ece_lugar_read))[-1]+1),]$X__1
          ),
      read =
        ece_notas_read[c(1,which(ece_notas_read=="Satisfactorio")+1),
                       ]$Satisfactorio
  ) %>% as_tibble %>%
    left_join(
      cbind(
        region =
          c("NACIONAL",
            ece_lugar_math[c(which(is.na(ece_lugar_math))[-1]+1),]$X__1),
        math =
          ece_notas_math[c(1,which(ece_notas_math=="Satisfactorio")+1),
                         ]$Satisfactorio
        ) %>% as_tibble,
      by = "region") %>%
    mutate(read =
             as.numeric(stri_replace_all(read, fixed = "%", replacement = "")),
           math =
             as.numeric(stri_replace_all(math, fixed = "%", replacement = ""))
           ) %>%
    mutate(global = (read+math)/2)
) %>%
  evaluate_plan(
    rules = list(MINEDUDIR__ = minedudir),
    expand = FALSE
  )