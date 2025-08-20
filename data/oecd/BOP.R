
library("tidyr")
library("dplyr")
library("data.table")
library("rsdmx")

# BOP ----------

BOP <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_BOP@DF_BOP,1.0" %>%
  readSDMX() %>%
  as_tibble

save(BOP, file = "BOP.RData")

# BOP_var --------

BOP_var <- "https://sdmx.oecd.org/public/rest/dataflow/OECD.SDD.TPS/DSD_BOP@DF_BOP/1.0?references=all" %>%
  readSDMX()
save(BOP_var, file = "BOP_var.RData")
