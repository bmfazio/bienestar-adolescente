# ejemplo original - https://www.r-bloggers.com/pisa-2015-how-to-readprocessplot-the-data-with-r/
library("foreign")
library("intsvy")
library("dplyr")
library("ggplot2")
library("data.table")
#stud2015 <- read.spss("CY6_MS_CMB_STU_QQQ.sav", use.value.labels = TRUE, to.data.frame = TRUE)
stud2015 <- fread("D:/datasets/pisa/2015/CY6_MS_CMB_STU_QQQ_PER.csv")
genderMath <- pisa2015.mean.pv(pvlabel = "MATH", by = c("CNT", "ST004D01T"), data = stud2015)

genderMath <- genderMath[,c(1,2,4,5)]
genderMath %>%
  select(CNT, ST004D01T, Mean) %>%
  spread(ST004D01T, Mean) -> genderMathWide
# test
pisa2015.ben.pv(pvlabel = "MATH", by = c("CNT", "ST004D01T"), data = stud2015)
pisa2015.ben.pv(pvlabel = "READ", by = c("CNT", "ST004D01T"), data = stud2015)
#

# Importantes interpretaciones:
# Los PVs (plausible values) son
# posterior draws de la habilidad del chaibol
# O sea con su mean determinarias puntaje y categorizacion

#setwd("D:/datasets/pisa/2015/")
library(readr)
library(intsvy)
library(data.table)
flt <- fread("D:/datasets/pisa/2015/cy6_ms_cmb_stu_flt_PER.csv")

apply(flt[,206:215], 1, mean) %>% range


# Level 2 es nivel "baseline"
# Reading cutoff - p375 PISA 2015 vol I
read_cutoff <- c(0, 262.04, 334.75, 407.47, 480.18, 552.89, 625.61, 698.32)
# Math cutoff - p388 PISA 2015 vol I
math_cutoff <- c(0, 357.77, 420.07, 482.38, 544.68, 606.99, 669.30)
# Financial cutoff - p75 PISA 2015 vol IV
fina_cutoff <- c(0, 326.00, 400.33, 475.10, 549.86, 624.63)

#pisa2015.mean.pv

#intsvy:::pisa2015_conf

list(
  variables =
    list(
      pvlabelpref = "PV",
      pvlabelsuff = "READ",
      weightFinal = "W_FSTUWT",
      weightBRR = "W_FSTURWT"
      ),
    parameters =
    list(
      cutoffs = c(357.77, 420.07, 482.38, 544.68, 606.99, 669.3),
      percentiles = c(5, 10, 25, 75, 90, 95),
      PVreps = 10,
      BRRreps = 80, 
      weights = "BRR",
      replication_scheme = "pisa")
  )

# pisa2015.mean.pv >>
#   intsvy.mean.pv >>
#     pv.input(pvnames = pvnames, data = data, config = config)

function (pvnames, by, data, export = FALSE, name = "output", 
    folder = getwd(), config) 
{
    pv.input <- function(pvnames, data, config) {
        if (nrow(data) <= 1) 
            return(data.frame(Freq = length(data[[config$variables$weightFinal]]), 
                Mean = NA, s.e. = NA, SD = NA, s.e = NA))
        if (config$parameters$weights %in% c("BRR", "mixed_piaac")) {
            R.mean <- sapply(pvnames, function(k) sapply(1:config$parameters$BRRreps, 
                function(i) weighted.mean(data[[k]], data[[paste0(config$variables$weightBRR, 
                  i)]], na.rm = TRUE)))
            R.sd <- sapply(pvnames, function(x) sapply(1:config$parameters$BRRreps, 
                function(i) (sum(data[[paste0(config$variables$weightBRR, 
                  i)]] * (data[[x]] - R.mean[i, x])^2, na.rm = TRUE)/sum(data[[paste0(config$variables$weightBRR, 
                  i)]], na.rm = TRUE))^(1/2)))
            PV.mean <- sapply(pvnames, function(x) weighted.mean(data[[x]], 
                data[[config$variables$weightFinal]], na.rm = TRUE))
            PV.sd <- sapply(pvnames, function(x) (sum(data[[config$variables$weightFinal]] * 
                (data[[x]] - PV.mean[x])^2, na.rm = TRUE)/sum(data[[config$variables$weightFinal]], 
                na.rm = TRUE))^(1/2))
            MEAN.m <- mean(PV.mean)
            SD.m <- mean(PV.sd)
            cc = 1/20

            var.mean.w <- mean(sapply(seq_along(pvnames), function(i) cc * 
                sum((R.mean[, i] - PV.mean[i])^2)))
            var.mean.b <- (1/(length(pvnames) - 1)) * sum(sapply(seq_along(pvnames), 
                function(i) (PV.mean[i] - MEAN.m)^2))
            mean.se <- (var.mean.w + (1 + 1/length(pvnames)) * 
                var.mean.b)^(1/2)
            var.sd.w <- mean(sapply(seq_along(pvnames), function(i) cc * 
                sum((R.sd[, i] - PV.sd[i])^2)))
            var.sd.b <- (1/(length(pvnames) - 1)) * sum(sapply(seq_along(pvnames), 
                function(i) (PV.sd[i] - SD.m)^2))
            sd.se <- (var.sd.w + (1 + 1/length(pvnames)) * var.sd.b)^(1/2)
            result <- data.frame(Freq = length(data[[config$variables$weightFinal]]), 
                Mean = mean(MEAN.m), s.e. = mean.se, SD = mean(SD.m), 
                s.e = sd.se)
        }

        return(round(result, 2))
    }
    if (missing(by)) {
        output <- pv.input(pvnames = pvnames, data = data, config = config)
    }
    else {
        for (i in by) {
            data[[c(i)]] <- as.factor(data[[c(i)]])
        }
        output <- ddply(data, by, function(x) pv.input(data = x, 
            pvnames = pvnames, config = config))
    }
    if (export) {
        write.csv(output, file = file.path(folder, paste(name, 
            ".csv", sep = "")))
    }
    class(output) <- c("intsvy.mean", "data.frame")
    return(output)
}