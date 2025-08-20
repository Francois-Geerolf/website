library("tidyr")
library("dplyr")
library("data.table")
library("rsdmx")

# PRICES_ALL --------

PRICES_ALL <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_PRICES@DF_PRICES_ALL," %>%
  readSDMX() %>%
  as_tibble

save(PRICES_ALL, file = "PRICES_ALL.RData")


# PRICES_ALL_var --------

PRICES_ALL_var <- "https://sdmx.oecd.org/public/rest/dataflow/OECD.SDD.TPS/DSD_PRICES@DF_PRICES_ALL/1.0?references=all" %>%
  readSDMX()

save(PRICES_ALL_var, file = "PRICES_ALL_var.RData")




