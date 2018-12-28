pisa_load <- drake_plan(
  pisa.flt =
    fread(
      file_in("DATADIR__/pisa/2015/cy6_ms_cmb_stu_flt_PER.csv")),
  pisa.qqq =
    fread(
      file_in("DATADIR__/pisa/2015/CY6_MS_CMB_STU_QQQ_PER.csv"))
) %>%
  evaluate_plan(
    rules = list(DATADIR__ = datadir),
    expand = FALSE
  )

## falta ver como aplicar los pesos
 # revisar la sintaxis de intsvy

library(intsvy)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(data.table)

pisaQQQ <- fread("D:/datasets/pisa/2015/CY6_MS_CMB_STU_QQQ_PER.csv")
pisaQQ2 <- fread("D:/datasets/pisa/2015/CY6_MS_CMB_STU_QQ2_PER.csv")
pisaFLT <- fread("D:/datasets/pisa/2015/cy6_ms_cmb_stu_flt_PER.csv")

# Financial LITERASI
pisaFLT$PV1FLIT

# satisfelingeneral 
pisaQQ2$ST016Q01NA %>% hist
# aspiracion educativa
pisa$ST111Q01TA %>% range(na.rm = T)
# aspiraciones generales

# sexo
pisa$ST001D01T
preguntas_tic <-
  c(
# A que edad usaste por primera vez una computadora?
    "IC003Q01TA",
# Cuando tengo problemas con un aparato digital, creo que yo puedo resolverlos
    "IC014Q08NA",
# Leo información sobre aparatos digitales para ser independiente cuando tengo que usarlos
    "IC015Q03NA"
    )


genderMath <- pisa2015.mean.pv(pvlabel = "MATH", by = c("CNT", "ST004D01T"), data = stud2015)


## Ejemplo
## Source: http://smarterpoland.pl/index.php/2016/12/pisa-2015-how-to-readprocessplot-the-data-with-r/
library("intsvy")
library("dplyr")
library("ggplot2")
library("tidyr")
 
stud2015 <- read.spss("CY6_MS_CMB_STU_QQQ.sav", use.value.labels = TRUE, to.data.frame = TRUE)
genderMath <- pisa2015.mean.pv(pvlabel = "MATH", by = c("CNT", "ST004D01T"), data = stud2015)
 
genderMath <- genderMath[,c(1,2,4,5)]
genderMath %>%
  select(CNT, ST004D01T, Mean) %>%
  spread(ST004D01T, Mean) -> genderMathWide
 
genderMathSelected <-
  genderMathWide %>%
  filter(CNT %in% c("Austria", "Japan", "Switzerland",  "Poland", "Singapore", "Finland", "Singapore", "Korea", "United States"))
 
pl <- ggplot(genderMathWide, aes(Female, Male)) +
  geom_point() +
  geom_point(data=genderMathSelected, color="red") +
  geom_text(data=genderMathSelected, aes(label=CNT), color="grey20") +
  geom_abline(slope=1, intercept = 0) + 
  geom_abline(slope=1, intercept = 20, linetype = 2, color="grey") + 
  geom_abline(slope=1, intercept = -20, linetype = 2, color="grey") +
  geom_text(x=425, y=460, label="Boys +20 points", angle=45, color="grey", size=8) + 
  geom_text(x=460, y=425, label="Girls +20 points", angle=45, color="grey", size=8) + 
  coord_fixed(xlim = c(400,565), ylim = c(400,565)) +
  theme_bw() + ggtitle("PISA 2015 in Math - Gender Gap") +
  xlab("PISA 2015 Math score for girls") +
  ylab("PISA 2015 Math score for boys") 