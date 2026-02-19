library("data.table")
library("tidyverse")
setwd("~/iCloud/website/data/meteo")

temp <-  tibble(date = seq.Date(from = as.Date("2015-01-01"), 
                                to = Sys.Date(), 
                                by = "day")) %>%
  mutate(url = paste0("https://www.meteo60.fr/stations-releves/station-jour-csv?station_id=-07156&date=", paste0(format(date, "%d/%m/%Y")))) %>%
  mutate(data = map(url, ~ fread(.)))
  
sapply(temp$data, function(df) class(df$`Vent moyen`))

temp2 <- temp %>%
  mutate(data = lapply(data, function(df) {
  df$`Vent moyen` <- as.character(df$`Vent moyen`)
  df$`V9` <- as.character(df$`V9`)
  df$`-` <- as.character(df$`-`)
  return(df)
}))



montsouris <- temp2 %>%
  unnest %>%
  select(-url) %>%
  select(1:14) %>%
  arrange(desc(date), desc(`Heure_legale`))

save(montsouris, file = "montsouris-jour.RData")

file.size("montsouris-jour.RData")

montsouris %>%
  head(10)

