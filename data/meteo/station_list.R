library("data.table")
library("tidyverse")
setwd("~/iCloud/website/data/meteo")

# 
# https://donneespubliques.meteofrance.fr/client/document/20140701_liste_stations_temps_reel_141_154.xls

curl::curl_download("https://donneespubliques.meteofrance.fr/client/document/20140701_liste_stations_temps_reel_141_154.xls",
              destfile = "20140701_liste_stations_temps_reel_141_154.xls")


station_list <- readxl::read_xls("20140701_liste_stations_temps_reel_141_154.xls", skip = 9) %>%
  slice(4:n())

save(station_list, file = "station_list.RData")
