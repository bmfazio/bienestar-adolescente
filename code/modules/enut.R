out.enut <- function() {
  # caracteristicas de miembros del hogar
  enut200 <- import("enut/Modulo126/02_CAPITULO_200.sav" %>% paste0(ineidir,.), setclass = "data.table")
  enut200[,id:=paste(CONGLOMERADO,NUMVIVREM,NSELV,HOGAR,P200_ID)]
  # # tareas de apoyo al hogar (otro pariente/no pariente alojado) (20 <= 18, 60 <= 24)
  # enut400 <- import("enut/Modulo128/04_CAPITULO_400.sav" %>% paste0(ineidir,.), setclass = "data.table")
  # enut400[,id:=paste(CONGLOMERADO,NUMVIVREM,NSELV,HOGAR,P400A)]
  # tareas realizadas para el hogar
  enut500 <- import("enut/Modulo129/05_CAPITULO_500.sav" %>% paste0(ineidir,.), setclass = "data.table")
  enut500[,id:=paste(CONGLOMERADO,NUMVIVREM,NSELV,HOGAR,P500_C)]
  
  # [2010] ENUT
  
  enut200[,.(id,
             sexo=putlabel(P204),edad=P205)] %>%
    merge(enut500[,.(id, psu = CONGLOMERADO, peso = FAC500,
                     # xA necesidad personales
                     # xB actividades educativas
                     # ?C actividades culinarias?
                     tiempo.hogar =
                       ifelse(P501C_1==1,P502C_1_H+P502C_1_M/60,0)%+rmna%
                       ifelse(P501C_2==1,P502C_2_H+P502C_2_M/60,0)%+rmna%
                       ifelse(P501C_3==1,P502C_3_H+P502C_3_M/60,0)%+rmna%
                       ifelse(P501C_4==1,P502C_4_H+P502C_4_M/60,0)%+rmna%
                       ifelse(P501C_5==1,P502C_5_H+P502C_5_M/60,0)%+rmna%
                       ifelse(P501C_6==1,P502C_6_H+P502C_6_M/60,0)%+rmna%
                       ifelse(P501C_7==1,P502C_7_H+P502C_7_M/60,0)%+rmna%
                       ifelse(P501C_8==1,P502C_8_H+P502C_8_M/60,0)%+rmna%
                         # ?D aseo de la vivienda?
                       ifelse(P501D_1==1,P502D_1_H+P502D_1_M/60,0)%+rmna%
                       ifelse(P501D_2==1,P502D_2_H+P502D_2_M/60,0)%+rmna%
                       ifelse(P501D_3==1,P502D_3_H+P502D_3_M/60,0)%+rmna%
                       ifelse(P501D_4==1,P502D_4_H+P502D_4_M/60,0)%+rmna%
                       ifelse(P501D_5==1,P502D_5_H+P502D_5_M/60,0)%+rmna%
                       ifelse(P501D_6==1,P502D_6_H+P502D_6_M/60,0)%+rmna%
                       ifelse(P501D_7==1,P502D_7_H+P502D_7_M/60,0)%+rmna%
                       ifelse(P501D_8==1,P502D_8_H+P502D_8_M/60,0)%+rmna%
                       ifelse(P501D_9==1,P502D_9_H+P502D_9_M/60,0)%+rmna%
                         # ?E cuidado y confeccion de ropa?
                       ifelse(P501E_1==1,P502E_1_H+P502E_1_M/60,0)%+rmna%
                       ifelse(P501E_2==1,P502E_2_H+P502E_2_M/60,0)%+rmna%
                       ifelse(P501E_3==1,P502E_3_H+P502E_3_M/60,0)%+rmna%
                       ifelse(P501E_4==1,P502E_4_H+P502E_4_M/60,0)%+rmna%
                       ifelse(P501E_5==1,P502E_5_H+P502E_5_M/60,0)%+rmna%
                       ifelse(P501E_6==1,P502E_6_H+P502E_6_M/60,0)%+rmna%
                       # F reparacion/construccion/mantenimiento de la vivienda
                       ifelse(P501F_1==1,P502F_1_H+P502F_1_M/60,0)%+rmna%
                       ifelse(P501F_2==1,P502F_2_H+P502F_2_M/60,0)%+rmna%
                       ifelse(P501F_3==1,P502F_3_H+P502F_3_M/60,0)%+rmna%
                       ifelse(P501F_4==1,P502F_4_H+P502F_4_M/60,0)%+rmna%
                       ifelse(P501F_5==1,P502F_5_H+P502F_5_M/60,0)%+rmna%
                       ifelse(P501F_6==1,P502F_6_H+P502F_6_M/60,0)%+rmna%
                       # G cuidado de bebes y otro
                       ifelse(P501G_1==1,P502G_1_H+P502G_1_M/60,0)%+rmna%
                       ifelse(P501G_2==1,P502G_2_H+P502G_2_M/60,0)%+rmna%
                       ifelse(P501G_3==1,P502G_3_H+P502G_3_M/60,0)%+rmna%
                       ifelse(P501G_4==1,P502G_4_H+P502G_4_M/60,0)%+rmna%
                       ifelse(P501G_5==1,P502G_5_H+P502G_5_M/60,0)%+rmna%
                       ifelse(P501G_6==1,P502G_6_H+P502G_6_M/60,0)%+rmna%
                       ifelse(P501G_7==1,P502G_7_H+P502G_7_M/60,0)%+rmna%
                       ifelse(P501G_8==1,P502G_8_H+P502G_8_M/60,0)%+rmna%
                       ifelse(P501G_9==1,P502G_9_H+P502G_9_M/60,0)%+rmna%
                       # H cuidado de enfermos
                       ifelse(P501H_1==1,P502H_1_H+P502H_1_M/60,0)%+rmna%
                       ifelse(P501H_2==1,P502H_2_H+P502H_2_M/60,0)%+rmna%
                       ifelse(P501H_3==1,P502H_3_H+P502H_3_M/60,0)%+rmna%
                       ifelse(P501H_4==1,P502H_4_H+P502H_4_M/60,0)%+rmna%
                       # I compras para el hogar
                       ifelse(P501I_1==1,P502I_1_H+P502I_1_M/60,0)%+rmna%
                       ifelse(P501I_2==1,P502I_2_H+P502I_2_M/60,0)%+rmna%
                       ifelse(P501I_3==1,P502I_3_H+P502I_3_M/60,0)%+rmna%
                       ifelse(P501I_4==1,P502I_4_H+P502I_4_M/60,0)%+rmna%
                       ifelse(P501I_5==1,P502I_5_H+P502I_5_M/60,0)%+rmna%
                       ifelse(P501I_6==1,P502I_6_H+P502I_6_M/60,0)%+rmna%
                       ifelse(P501I_7==1,P502I_7_H+P502I_7_M/60,0)%+rmna%
                       ifelse(P501I_8==1,P502I_8_H+P502I_8_M/60,0)%+rmna%
                       ifelse(P501I_9==1,P502I_9_H+P502I_9_M/60,0)%+rmna%
                       # J gerencia del hoga
                       ifelse(P501J_1==1,P502J_1_H+P502J_1_M/60,0)%+rmna%
                       ifelse(P501J_2==1,P502J_2_H+P502J_2_M/60,0)%+rmna%
                       ifelse(P501J_3==1,P502J_3_H+P502J_3_M/60,0)%+rmna%
                       ifelse(P501J_4==1,P502J_4_H+P502J_4_M/60,0)%+rmna%
                       ifelse(P501J_5==1,P502J_5_H+P502J_5_M/60,0)%+rmna%
                       ifelse(P501J_6==1,P502J_6_H+P502J_6_M/60,0)%+rmna%
                       ifelse(P501J_7==1,P502J_7_H+P502J_7_M/60,0)%+rmna%
                       ifelse(P501J_8==1,P502J_8_H+P502J_8_M/60,0)%+rmna%
                       ifelse(P501J_9==1,P502J_9_H+P502J_9_M/60,0)%+rmna%
                       ifelse(P501J_10==1,P502J_10_H+P502J_10_M/60,0)%+rmna%
                       ifelse(P501J_11==1,P502J_11_H+P502J_11_M/60,0)%+rmna%
                       ifelse(P501J_12==1,P502J_12_H+P502J_12_M/60,0)%+rmna%
                       ifelse(P501J_13==1,P502J_13_H+P502J_13_M/60,0)%+rmna%
                       # xK familia y sociabilidad
                       # xL tiempo libre
                       # ?M cuidado de huertos y crianza de animales (NO actividad economica)
                       ifelse(P501M_1==1,P502M_1_H+P502M_1_M/60,0)%+rmna%
                       ifelse(P501M_2==1,P502M_2_H+P502M_2_M/60,0)%+rmna%
                       ifelse(P501M_3==1,P502M_3_H+P502M_3_M/60,0)%+rmna%
                       ifelse(P501M_4==1,P502M_4_H+P502M_4_M/60,0)%+rmna%
                       # ?N apoyo a otro hogar (no remunerado)
                       ifelse(P501N_1==1,P502N_1_H+P502N_1_M/60,0)%+rmna%
                       ifelse(P501N_2==1,P502N_2_H+P502N_2_M/60,0)%+rmna%
                       ifelse(P501N_3==1,P502N_3_H+P502N_3_M/60,0)%+rmna%
                       ifelse(P501N_4==1,P502N_4_H+P502N_4_M/60,0)%+rmna%
                       ifelse(P501N_5==1,P502N_5_H+P502N_5_M/60,0)%+rmna%
                       ifelse(P501N_6==1,P502N_6_H+P502N_6_M/60,0)%+rmna%
                       ifelse(P501N_7==1,P502N_7_H+P502N_7_M/60,0)%+rmna%
                       ifelse(P501N_8==1,P502N_8_H+P502N_8_M/60,0)%+rmna%
                       ifelse(P501N_9==1,P502N_9_H+P502N_9_M/60,0)%+rmna%
                       ifelse(P501N_10==1,P502N_10_H+P502N_10_M/60,0)%+rmna%
                       ifelse(P501N_11==1,P502N_11_H+P502N_11_M/60,0)%+rmna%
                       # xO trabajo voluntario en organizaciones
                       # P cuidado de miembros del hogar
                       ifelse(P501P_1==1,P502P_1_H+P502P_1_M/60,0)%+rmna%
                       ifelse(P501P_2==1,P502P_2_H+P502P_2_M/60,0)%+rmna%
                       ifelse(P501P_3==1,P502P_3_H+P502P_3_M/60,0)%+rmna%
                       ifelse(P501P_4==1,P502P_4_H+P502P_4_M/60,0)%+rmna%
                       ifelse(P501P_5==1,P502P_5_H+P502P_5_M/60,0)%+rmna%
                       ifelse(P501P_6==1,P502P_6_H+P502P_6_M/60,0)%+rmna%
                       ifelse(P501P_7==1,P502P_7_H+P502P_7_M/60,0)%+rmna%
                       ifelse(P501P_8==1,P502P_8_H+P502P_8_M/60,0)%+rmna%
                       ifelse(P501P_9==1,P502P_9_H+P502P_9_M/60,0)%+rmna%
                       ifelse(P501P_10==1,P502P_10_H+P502P_10_M/60,0),
                     tiempo.libre =
                       # Tiempo en familia/social
                       ifelse(P501K_1==1,P502K_1_H+P502K_1_M/60,0)%+rmna%
                       ifelse(P501K_2==1,P502K_2_H+P502K_2_M/60,0)%+rmna%
                       ifelse(P501K_3==1,P502K_3_H+P502K_3_M/60,0)%+rmna%
                       ifelse(P501K_4==1,P502K_4_H+P502K_4_M/60,0)%+rmna%
                       ifelse(P501K_5==1,P502K_5_H+P502K_5_M/60,0)%+rmna%
                       ifelse(P501K_6==1,P502K_6_H+P502K_6_M/60,0)%+rmna%
                       # Tiempo en actividad recreacional
                       ifelse(P501L_1==1,P502L_1_H+P502L_1_M/60,0)%+rmna%
                       ifelse(P501L_2==1,P502L_2_H+P502L_2_M/60,0)%+rmna%
                       ifelse(P501L_3==1,P502L_3_H+P502L_3_M/60,0)%+rmna%
                       ifelse(P501L_4==1,P502L_4_H+P502L_4_M/60,0)%+rmna%
                       ifelse(P501L_5==1,P502L_5_H+P502L_5_M/60,0)%+rmna%
                       ifelse(P501L_6==1,P502L_6_H+P502L_6_M/60,0)%+rmna%
                       ifelse(P501L_7==1,P502L_7_H+P502L_7_M/60,0)%+rmna%
                       ifelse(P501L_8==1,P502L_8_H+P502L_8_M/60,0)%+rmna%
                       ifelse(P501L_9==1,P502L_9_H+P502L_9_M/60,0)%+rmna%
                       ifelse(P501L_10==1,P502L_10_H+P502L_10_M/60,0)%+rmna%
                       ifelse(P501L_11==1,P502L_11_H+P502L_11_M/60,0)%+rmna%
                       ifelse(P501L_12==1,P502L_12_H+P502L_12_M/60,0),
                     sindicato = 2-P501O_5,
                     voluntario = ((2-P501O_1)%+rmna%(2-P501O_2)%+rmna%(2-P501O_3)%+rmna%(2-P501O_4)%+rmna%(2-P501O_5))>0,
                     cetpro = ((2-P501B_4)%+rmna%(2-P501B_5)>0)
                     )],
          by = "id") %>%
      svydesign(ids=~psu, weights=~peso, data=.) -> denut
  
  list(
    # Tiempo dedicado a tareas del hogar (no remunerado) [horas/semana]
    denut %>% subset(12<=edad&edad<=17) %>% svymean( ~ tiempo.hogar, .),
    # Porcentaje de adolescentes que participan en actividades recreacionales o sociales por un periodo específico durante el día o la semana
    denut %>% subset(12<=edad&edad<=17) %>% svymean( ~ tiempo.libre, .),    
    # Participación de adolescentes en sindicatos
    denut %>% subset(12<=edad&edad<=17) %>% svyciprop( ~ sindicato, .),
    # Indicador de voluntariado
    denut %>% subset(12<=edad&edad<=17) %>% svyciprop( ~ voluntario, .),
    # CETPRO 500-B4
    denut %>% subset(15<=edad&edad<=19) %>% svyciprop( ~ cetpro, .)
    # Participación en movimientos formales y no formales
    # No hay algo que no sea redundante con voluntariado
    )  %>% lapply(svy2pci) %>% do.call(rbind, .) -> tmp.estimates
  
    colnames(tmp.estimates) <- c("valor", "CI95.Inf", "CI95.Sup")
  
  indnom <- c(
    "(12-17 años) Tiempo dedicado a tareas del hogar (no remunerado) [horas/semana]",
    "(12-17 años) Tiempo recreacional/social [horas/semana]",
    "(12-17 años) Participación de adolescentes en sindicatos",
    "(12-17 años) Participación de adolescentes en trabajo voluntario",
    "(15-19 años) Estudia en CETPRO"
  )
  
  "Documento de Pablo indica a partir de 10 años, pero aqui solo hay desde 12. Falta detallar voluntariado, movimiento formales y actividades recreacionales" -> comments
  
  cbind(indnom,
        data.frame(round(tmp.estimates*c(1,1,rep(100,nrow(tmp.estimates)-2)),2)),
        fuente = "ENUT 2010") %>% list(content=., comments=comments)
}

out.enut <- out.enut()