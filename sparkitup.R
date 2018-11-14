library(haven)
library(readr)
library(dplyr)

flt_file <- "D:/datasets/pisa/2015/PUF_SAS_COMBINED_CMB_STU_FLT/cy6_ms_cmb_stu_flt.sas7bdat"

flt <- read_sas(flt_file) %>% filter(CNT == "PER")

write_csv(flt, "D:/datasets/pisa/2015/CSV/flt_per.csv")

###
qqq_file1 <- "D:/datasets/pisa/2015/CSV/qqq/part-00040-01d07651-3143-4fa5-b265-c9487510be43-c000.csv"
qqq_file3 <- "D:/datasets/pisa/2015/CSV/qqq/part-00039-01d07651-3143-4fa5-b265-c9487510be43-c000.csv"

qqq1 <- read_csv(qqq_file1)
qqq2 <- read_csv(qqq_file3)

qqq <- bind_rows(qqq1, qqq2)

write_csv(qqq, "D:/datasets/pisa/2015/CSV/qqq_per.csv")

###
cog_file1 <- "D:/datasets/pisa/2015/CSV/cog/part-00097-f5b8c567-e45b-4071-a48c-d3c261b777a9-c000.csv"
cog_file2 <- "D:/datasets/pisa/2015/CSV/cog/part-00095-f5b8c567-e45b-4071-a48c-d3c261b777a9-c000.csv"
cog_file3 <- "D:/datasets/pisa/2015/CSV/cog/part-00096-f5b8c567-e45b-4071-a48c-d3c261b777a9-c000.csv"

cog1 <- read_csv(cog_file1)
cog2 <- read_csv(cog_file2)
cog3 <- read_csv(cog_file3)

write_csv(bind_rows(cog1, cog2, cog3), "D:/datasets/pisa/2015/CSV/cog_per.csv")