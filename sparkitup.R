# library(rio)
# library(haven)
# library(data.table)
# pisa_cog_file <- "/home/bmfazio/Documents/datasets/pisa/2015/PUF_SAS_COMBINED_CMB_STU_COG/cy6_ms_cmb_stu_cog.sas7bdat"
# 
# pisa_cog <- import(pisa_cog_file, setclass = "data.table")

# El archivo de arriba es dms grande para memoria.
# Explorare si Spark tiene herramientas que me permitan lidiar con el.


install.packages("sparklyr")
library(sparklyr)
spark_install(version = "2.4.0")