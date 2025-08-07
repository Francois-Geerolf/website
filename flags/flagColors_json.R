rm(list = ls())
pklist <- c("tidyverse", "curl", "fredr", "data.table", "pryr", 
            "benchmarkme", "RJSONIO")
source("https://fgeerolf.com/code/load_pk.R")
setwd("~/iCloud/website/data/flags")

# https://www.schemecolor.com/germany-flag-colors.php

curl_download("https://raw.githubusercontent.com/reimertz/flag-colors/master/data/flagColors.json",
              destfile = "flagColors.json",
              quiet = F)

flagColors_json <- "flagColors.json" %>%
  rjson::fromJSON(file = .)

save(flagColors_json, file = "flagColors_json.RData")
