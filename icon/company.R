rm(list = ls())
pklist <- c("tidyverse", "RCurl", "httr", "countrycode", "curl")
setwd("~/Dropbox/icon/company_logo/")
source("../../code/load_unload_pk.R")
source("../../code/load_data.R")

load_data("r-g/marketcap.RData")

list_tickers <- marketcap %>%
  mutate(Ticker = sub(".*\n ", "", Name),
         Ticker = str_trim(Ticker)) %>%
  select(Ticker) %>%
  pull(Ticker)

for (ticker in list_tickers){
  url <- paste0("https://companiesmarketcap.com/img/company-logos/64/", ticker, ".png")
  curl_download(url = url,
                destfile = paste0(ticker, ".png"),
                quiet = F)
}

