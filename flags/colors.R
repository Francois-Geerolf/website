rm(list = ls())
pklist <- c("tidyverse", "curl")
setwd("~/iCloud/website/data/flags")

source("../../code/load_unload_pk.R")
source("../../code/load_data.R")
offline <- T

load_data("flags/flagColors_data.RData")

africa <- tibble(country = "Algeria", color = "#006233", color2 = "") %>%
  add_row(country = "Angola", color = "#000000", color2 = "#CE1126") %>%
  add_row(country = "Gabon", color = "#FCD116", color2 = "#3A75C4") %>%
  add_row(country = "Ivory Coast", color = "#F77F00", color2 = "") %>%
  add_row(country = "Saudi Arabia", color = "#006C35", color2 = "") %>%
  add_row(country = "South Africa", color = "#007A4D", color2 = "") %>%
  add_row(country = "Tunisia", color = "#E70013", color2 = "") %>%
  add_row(country = "Sub-Saharan Africa", color = "#000000", color2 = "")
  
  



america <- tibble(country = "Argentina", color = "#74ACDF", color2 = "") %>%
  add_row(country = "Brazil", color = "#009B3A", color2 = "") %>%
  add_row(country = "Canada", color = "#FF0000", color2 = "") %>%
  add_row(country = "Chile", color = "#0039A6", color2 = "") %>%
  add_row(country = "Colombia", color = "#FCD116", color2 = "#CE1126") %>%
  add_row(country = "Mexico", color = "#006847", color2 = "#CE1126") %>%
  add_row(country = "Peru", color = "#D91023", color2 = "") %>%
  add_row(country = "United States", color = "#3C3B6E", color2 = "#B22234") %>%
  add_row(country = "Uruguay", color = "#9E830E", color2 = "") %>%
  add_row(country = "Venezuela", color = "#FFCC00", color2 = "#CF142B")

asia <- tibble(country = "China", color = "#FFDC00", color2 = "#DE2910") %>%
  add_row(country = "Hong Kong", color = "#DE2408", color2 = "") %>%
  add_row(country = "India", color = "#FF9933", color2 = "#128807") %>%
  add_row(country = "Indonesia", color = "#CE1126", color2 = "#CE1126") %>%
  add_row(country = "Israel", color = "#0038B8", color2 = "") %>%
  add_row(country = "Japan", color = "#BC002D", color2 = "") %>%
  add_row(country = "Korea", color = "#030303", color2 = "") %>%
  add_row(country = "Malaysia", color = "#CC0001", color2 = "") %>%
  add_row(country = "Philippines", color = "#0038A8", color2 = "#CE1126") %>%
  add_row(country = "Russia", color = "#D52B1E", color2 = "#0039A6") %>%
  add_row(country = "Singapore", color = "#ED2939", color2 = "#F58C94") %>%
  add_row(country = "South Korea", color = "#030303", color2 = "") %>%
  add_row(country = "Thailand", color = "#6D688A", color2 = "")

australia <- tibble(country = "Australia", color = "#00008B", color2 = "") %>%
  add_row(country = "New Zealand", color = "#CC142B", color2 = "")

europe <- tibble(country = "Austria", color = "#ED2939", color2 = "") %>%
  add_row(country = "Belgium", color = "#FAE042", color2 = "#ED2939") %>%
  add_row(country = "Bulgaria", color = "#00966E", color2 = "#D62612") %>%
  add_row(country = "Cyprus", color = "#D47600", color2 = "#475429") %>%
  add_row(country = "Czech Republic", color = "#11457E", color2 = "#D7141A") %>%
  add_row(country = "Czechia", color = "#11457E", color2 = "#D7141A") %>%
  add_row(country = "Croatia", color = "#FF0000", color2 = "#171796") %>%
  add_row(country = "Denmark", color = "#C60C30", color2 = "") %>%
  add_row(country = "Estonia", color = "#4891D9", color2 = "#000000") %>%
  add_row(country = "Europe", color = "#003399", color2 = "#FFCC00") %>%
  add_row(country = "European Union", color = "#003399", color2 = "#FFCC00") %>%
  add_row(country = "Euro Area", color = "#003399", color2 = "#FFCC00") %>%
  add_row(country = "Finland", color = "#003580", color2 = "") %>%
  add_row(country = "France", color = "#ED2939", color2 = "#002395") %>%
  add_row(country = "Germany", color = "#000000", color2 = "#DD0000") %>%
  add_row(country = "Greece", color = "#0D5EAF", color2 = "") %>%
  add_row(country = "Hungary", color = "#436F4D", color2 = "") %>%
  add_row(country = "Iceland", color = "#D72828", color2 = "#003897") %>%
  add_row(country = "Ireland", color = "#FF883E", color2 = "") %>%
  add_row(country = "Italy", color = "#009246", color2 = "") %>%
  add_row(country = "Netherlands", color = "#AE1C28", color2 = "#21468B") %>%
  add_row(country = "Norway", color = "#EF2B2D", color2 = "#002868") %>%
  add_row(country = "Poland", color = "#DC143C", color2 = "") %>%
  add_row(country = "Portugal", color = "#006600", color2 = "#FF0000") %>%
  add_row(country = "Romania", color = "#FCD116", color2 = "") %>%
  add_row(country = "Serbia", color = "#C6363C", color2 = "#0C4076") %>%
  add_row(country = "Slovak Republic", color = "#EE1C25", color2 = "#0B4EA2") %>%
  add_row(country = "Spain", color = "#FFC400", color2 = "#C60B1E") %>%
  add_row(country = "Sweden", color = "#FECC00", color2 = "#21468B") %>%
  add_row(country = "Switzerland", color = "#FF0000", color2 = "") %>%
  add_row(country = "Turkey", color = "#E30A17", color2 = "") %>%
  add_row(country = "United Kingdom", color = "#CF142B", color2 = "")

middle_east <- tibble(country = "Kuwait", color = "#007A3D", color2 = "#CE1126") %>%
  add_row(country = "Iraq", color = "#E9949D", color2 = "#000000") %>%
  add_row(country = "Iran", color = "#239F40", color2 = "#DA0101") %>%
  add_row(country = "Libya", color = "#000000", color2 = "#E70013")
  
  
groups <- tibble(country = "OECD", color = "#96BA13", color2 = "#086199") %>%
  add_row(country = "World", color = "#000000", color2 = "") %>%
  add_row(country = "OECD members", color = "#96BA13", color2 = "#086199")


  
colors_manual <- africa %>%
  bind_rows(america) %>%
  bind_rows(asia) %>%
  bind_rows(australia) %>%
  bind_rows(europe) %>%
  bind_rows(middle_east) %>%
  bind_rows(groups)



colors_automatic <- flagColors_data %>%
  filter(!(country %in% colors_manual$country)) %>%
  select(country, color = c1, color2 = c2)

colors <- colors_manual %>%
  bind_rows(colors_automatic) %>%
  arrange(country)

save(colors, file = "colors.RData")

system("sh _to_website_offline.sh")
