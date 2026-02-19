library("data.table")
library("tidyverse")
setwd("~/iCloud/website/data/meteo")

first_year <- 15
last_year <- year(Sys.Date())-2000
last_month <- month(Sys.Date())
total_years <- last_year-first_year

last_data <- fread(paste0("https://www.meteo60.fr/stations-releves/station-mois-csv?station_id=07156&mois=", last_month, "&annee=", last_year)) %>%
  select(-V7) %>%
  mutate_at(vars(-1), funs(as.character(.)))

last_data

temp <- tibble(annee = rep(first_year:last_year, times = c(rep(12, total_years), last_month)),
                     mois = c(rep(sprintf("%02d", 1:12), total_years), sprintf("%02d", 1:last_month))) %>%
  mutate(url = paste0("https://www.meteo60.fr/stations-releves/station-mois-csv?station_id=07156&mois=", mois, "&annee=", annee)) %>%
  mutate(data = map(url, ~ fread(.) %>%
                      as_tibble(., .name_repair = "unique") %>%
                      dplyr::select(-dplyr::any_of("V7")) %>%
                      mutate_at(vars(-1), funs(as.character(.)))))

montsouris <- temp %>%
  unnest %>%
  select(-url) %>%
  mutate_at(vars(-3, -8, -annee), funs(as.numeric(.))) %>%
  #filter(!is.na(mean_temp)) %>%
  mutate(jour = parse_number(Jour),
         annee = paste0("20", annee) %>% as.numeric,
         date = as.Date(ISOdate(annee, mois, jour))) %>%
  select(-mois, - annee, -jour) %>%
  select(date, Jour, everything()) %>%
  arrange(desc(date))

save(montsouris, file = "montsouris.RData")

file.size("montsouris.RData")
