rm(list = ls())
pklist <- c("tidyverse", "RCurl", "httr", "countrycode", "curl")
setwd("~/Dropbox/icon/flag/")
source("../../code/load_unload_pk.R")
source("../../code/load_data.R")

code_names <- codelist %>%
  filter(!is.na(country.name.en)) %>%
  mutate(country.name.en = gsub(" ", "-", country.name.en),
         country.name.en = str_to_lower(country.name.en)) %>%
  pull(country.name.en)

# https://cdn.countryflags.com/thumbs/czech-republic/flag-800.png
code_names <- c(code_names, "united-states-of-america", "czech-republic", "europe", "hong-kong")
code_names <- c("hongkong")

# Normal ------

setwd("~/Dropbox/icon/flag")

for (country in code_names){
  url_countryflags <- paste0("https://cdn.countryflags.com/thumbs/", country, "/flag-800.png")
  if (httr::HEAD(url_countryflags)$url == url_countryflags){
    cat("Downloading: ", country, "\n")
    curl_download(url = url_countryflags,
                  destfile = paste0(country, ".png"),
                  quiet = F)
  } else{
    cat("Redirection to https://cdn.countryflags.com/. Does not exist: ", country, "\n")
  }
}

# Round ------

setwd("~/Dropbox/icon/flag/round")

for (country in code_names){
  url_countryflags <- paste0("https://cdn.countryflags.com/thumbs/", country, "/flag-round-500.png")
  if (httr::HEAD(url_countryflags)$url == url_countryflags){
    cat("Downloading: ", country, "\n")
    curl_download(url = url_countryflags,
                  destfile = paste0(country, ".png"),
                  quiet = F)
  } else{
    cat("Redirection to https://cdn.countryflags.com/. Does not exist: ", country, "\n")
  }
}

# Waving ------

setwd("~/Dropbox/icon/flag/waving")

for (country in code_names){
  url_countryflags <- paste0("https://cdn.countryflags.com/thumbs/", country, "/flag-waving-500.png")
  if (httr::HEAD(url_countryflags)$url == url_countryflags){
    cat("Downloading: ", country, "\n")
    curl_download(url = url_countryflags,
                  destfile = paste0(country, ".png"),
                  quiet = F)
  } else{
    cat("Redirection to https://cdn.countryflags.com/. Does not exist: ", country, "\n")
  }
}

# Square ------

setwd("~/Dropbox/icon/flag/square")

for (country in code_names){
  url_countryflags <- paste0("https://cdn.countryflags.com/thumbs/", country, "/flag-square-500.png")
  if (httr::HEAD(url_countryflags)$url == url_countryflags){
    cat("Downloading: ", country, "\n")
    curl_download(url = url_countryflags,
                  destfile = paste0(country, ".png"),
                  quiet = F)
  } else{
    cat("Redirection to https://cdn.countryflags.com/. Does not exist: ", country, "\n")
  }
}

