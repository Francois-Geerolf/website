#!/usr/bin/env Rscript
# Récupère le catalogue complet d'opendata.paris.fr (API Opendatasoft
# Explore v2.1) et l'écrit, sous forme allégée, dans data/paris/catalogue.csv.
# data/paris/ vit uniquement dans ~/iCloud (pas dans le dépôt git) : à
# relancer à la main de temps en temps, pas de CI.
#   cd ~/iCloud/website/data/paris && Rscript _catalogue.R
suppressMessages({library(dplyr); library(readr); library(stringr)})

url <- "https://opendata.paris.fr/api/explore/v2.1/catalog/exports/csv?delimiter=%3B"
dest <- here::here("data", "paris", "catalogue.csv")

raw <- readr::read_delim(url, delim = ";", show_col_types = FALSE, progress = FALSE)
message(nrow(raw), " jeux de données récupérés")

strip_html <- function(x) {
  x |>
    str_replace_all("<[^>]+>", " ") |>
    str_replace_all("&nbsp;", " ") |>
    str_replace_all("&amp;", "&") |>
    str_replace_all("&#39;|&rsquo;", "'") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

catalogue <- raw |>
  transmute(
    id            = datasetid,
    titre         = `default.title`,
    theme         = `default.theme`,
    mots_cles     = `default.keyword`,
    producteur    = `default.publisher`,
    type_prod     = `dcat.publisher_type`,
    frequence     = `default.update_frequency`,
    licence       = `default.license`,
    modifie       = as.Date(`default.modified`),
    lignes        = as.integer(`default.records_count`),
    description   = strip_html(`default.description`) |> substr(1, 600)
  ) |>
  arrange(desc(modifie))

readr::write_csv(catalogue, dest, na = "")
message("écrit : ", dest, " (", nrow(catalogue), " lignes)")
