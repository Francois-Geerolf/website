rm(list = ls())
pklist <- c("tidyverse", "curl")
setwd("~/iCloud/website/data/flags")
source("../../code/load_unload_pk.R")
source("../../code/load_data.R")
offline <- T

load_data("flags/flagColors_json.RData")

flagColors_data <- tibble(country = rep("", 206)) %>%
  mutate(c1 = "",
         c2 = "",
         c3 = "",
         c4 = "",
         c5 = "",
         c6 = "",
         p1 = 0,
         p2 = 0,
         p3 = 0,
         p4 = 0,
         p5 = 0,
         p6 = 0)



for (i in 1:206){
  flagColors_data[i , "country"] <- flagColors_json[[i]]$name
  flagColors_data[i , "c1"] <- flagColors_json[[i]]$colors[[1]]$hex
  flagColors_data[i , "p1"] <- flagColors_json[[i]]$colors[[1]]$p
  flagColors_data[i , "c2"] <- flagColors_json[[i]]$colors[[2]]$hex
  flagColors_data[i , "p2"] <- flagColors_json[[i]]$colors[[2]]$p
  if (length(flagColors_json[[i]]$colors) > 2){
    flagColors_data[i , "c3"] <- flagColors_json[[i]]$colors[[3]]$hex
    flagColors_data[i , "p3"] <- flagColors_json[[i]]$colors[[3]]$p
  }
  if (length(flagColors_json[[i]]$colors) > 3){
    flagColors_data[i , "c4"] <- flagColors_json[[i]]$colors[[4]]$hex
    flagColors_data[i , "p4"] <- flagColors_json[[i]]$colors[[4]]$p
  }
  if (length(flagColors_json[[i]]$colors) > 4){
    flagColors_data[i , "c5"] <- flagColors_json[[i]]$colors[[5]]$hex
    flagColors_data[i , "p5"] <- flagColors_json[[i]]$colors[[5]]$p
  }
  if (length(flagColors_json[[i]]$colors) > 5){
    flagColors_data[i , "c6"] <- flagColors_json[[i]]$colors[[6]]$hex
    flagColors_data[i , "p6"] <- flagColors_json[[i]]$colors[[6]]$p
  }
}


flagColors_data <- flagColors_data %>%
  add_row(country = "Europe", c1 = "#003399", c2 = "#FFCC00") %>%
  add_row(country = "Hong Kong", c1 = "#DE2408", c2 = "#FFFFFF") %>%
  add_row(country = "OECD Members", c1 = "#96BA13", c2 = "#086199", c3 = "#AFCDDB", c4 = "#6F7173") %>%
  mutate_all(funs(ifelse(is.na(.), "", .)))

save(flagColors_data, file = "flagColors_data.RData")

system("sh _to_website_offline.sh")
