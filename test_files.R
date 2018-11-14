library(rio)
library(haven)
library(data.table)

gshs_file <- "D:/datasets/gshs/2010/PEH2010_Public_Use.sav"
gshs <- import(gshs_file, setclass = "data.table")