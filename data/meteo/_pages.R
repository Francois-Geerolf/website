#!/usr/bin/env Rscript
# Génère toutes les pages .qmd de data/meteo/ à partir de `meteo_stations`
# (code/meteo.R) : index.qmd, une page par station, stations.qmd,
# station_list.qmd, et des redirections pour les anciennes URL.
#
# Ajouter / retirer une station : modifier `meteo_stations` dans code/meteo.R,
# ajouter l'ancienne page dans `redirects` ci-dessous si on en retire une, puis
#   cd ~/github/website && Rscript data/meteo/_pages.R
# Les pages sont recompilées toutes les heures par .github/workflows/themes.yml.

source(here::here("code", "meteo.R"))
dir <- here::here("data", "meteo")

# Anciennes pages -> nouvelle cible
redirects <- c("montsouris"      = "paris-montsouris.html",
               "montsouris-jour" = "paris-montsouris.html",
               "lille"           = "index.html",
               "chamonix"        = "index.html")

yaml <- function(title, back = "Toutes les stations") paste0('---
title: "', title, '"
lang: fr
subtitle: \'<img src="https://fgeerolf.com/data/logos/meteo.png" alt="" style="height:1em;width:1em;object-fit:contain;vertical-align:-0.15em;margin-right:.35em">Données - Météo\'
author:
  - name: François Geerolf
    url: https://fgeerolf.com
format:
  html:
    code-fold: true
    toc: true
    theme: [flatly, ../_fgeerolf.scss]
    embed-resources: false
    other-links:
      - text: "', back, '"
        href: "https://fgeerolf.com/data/meteo/"
      - text: "Autres données"
        href: "https://fgeerolf.com/data/"
        icon: layers
      - href: "https://fgeerolf.com/"
        text: "Accueil"
        icon: house
    include-after-body: ../../statcounter.html
editor: source
---
')

setup <- function(extra = "") paste0('
<!-- Généré par data/meteo/_pages.R : modifier le générateur, pas ce fichier. -->

```{r, echo = FALSE, message = FALSE, warning = FALSE}
source(here::here("_rinit.R"))
invisible(Sys.setlocale("LC_TIME", "fr_FR.UTF-8"))
invisible(Sys.setenv(TZ = "Europe/Paris"))
source(here::here("code", "meteo.R"))
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      dpi = 150, fig.align = "center", out.width = "560px")
', extra, '
```
')

footer <- '
Sources : [meteo60.fr](https://www.meteo60.fr/stations-releves/) (stations Météo-France) ; [Iowa Environmental Mesonet](https://mesonet.agron.iastate.edu/) (stations ASOS/METAR hors de France).

```{r}
#| output: asis
page_compile_line()
```
'

chunk <- function(code, w = NULL, h = NULL, asis = FALSE) {
  opts <- if (!is.null(w)) sprintf("r, fig.width = %s, fig.height = %s", w, h) else "r"
  paste0("\n```{", opts, "}\n", if (asis) "#| output: asis\n", code, "\n```\n")
}

write_qmd <- function(name, txt) {
  writeLines(txt, file.path(dir, paste0(name, ".qmd")), useBytes = TRUE)
  message("✅ ", name, ".qmd")
}

# --- index.qmd ---------------------------------------------------------------
idx <- paste0(
  yaml("Météo", back = "Données Météo"),
  setup('stations <- setNames(meteo_stations$court, meteo_stations$id)
daily <- meteo_daily(stations, 30)'),
  "\n# Pages\n", chunk("meteo_pages_reactable()"),
  "\n# Comparaison par jour\n\n## Températures maximales, 30 derniers jours\n",
  chunk('meteo_plot_heatmap(daily, "tmax")', 5, 9.5),
  "\n## Température moyenne\n",
  chunk('meteo_plot_lines(daily, "tmoy")', 5, 4),
  paste0(purrr::pmap_chr(meteo_stations, \(id, slug, court, nom, ...) paste0(
    "\n# ", nom, "\n",
    chunk(sprintf('cat(meteo_stations$lieu[meteo_stations$slug == "%s"], " · [**Page détaillée →**](%s.html)\\n")', slug, slug), asis = TRUE),
    chunk(sprintf('meteo_plot_hours("%s", "%s — 48 dernières heures")', id, nom), 5, 6.5),
    chunk(sprintf('meteo_plot_days(dplyr::filter(daily, station == "%s"), "%s — 30 derniers jours")', court, nom), 5, 6)
  )), collapse = ""),
  footer)
write_qmd("index", idx)

# --- une page par station ------------------------------------------------------
purrr::pwalk(meteo_stations, \(id, slug, court, nom, ...) write_qmd(slug, paste0(
  yaml(paste("Météo —", nom)),
  setup(sprintf('st <- meteo_station_info("%s")
h7 <- meteo_hourly(st$id, from = Sys.Date() - 7)
daily <- meteo_daily(setNames(st$court, st$id), 400)
last30 <- dplyr::filter(daily, date > Sys.Date() - 30)', slug)),
  chunk('cat(st$lieu, " · station ", st$id, "\\n\\n", meteo_now_md(h7), "\\n", sep = "")', asis = TRUE),
  "\n# 7 derniers jours, heure par heure\n",
  chunk(sprintf('meteo_plot_hours(h = h7, title = "%s — 7 derniers jours", detail = TRUE)', nom), 5, 9.5),
  "\n# 30 derniers jours\n",
  chunk(sprintf('meteo_plot_days(last30, "%s — 30 derniers jours")', nom), 5, 6),
  "\n# Par rapport à l'an dernier\n",
  chunk('meteo_plot_vs_last_year(daily, 30, paste("30 derniers jours vs mêmes jours en", format(Sys.Date() - 365, "%Y")))', 5, 3.8),
  "\n# 12 derniers mois\n",
  chunk(sprintf('meteo_plot_year(daily, "%s — 12 derniers mois")', nom), 5, 6),
  '\n# Tableaux\n\n::: {.callout-note collapse="true"}\n## Heure par heure, 7 derniers jours\n',
  chunk("meteo_table_hours(h7)"),
  ':::\n\n::: {.callout-note collapse="true"}\n## Jour par jour, 30 derniers jours\n',
  chunk("meteo_table_days(last30)"),
  ":::\n\n[← Toutes les stations](index.html)\n",
  footer)))

# --- stations.qmd : toutes les stations meteo60, hier ---------------------------
write_qmd("stations", paste0(
  yaml("Météo — Toutes les stations, hier"),
  setup("d <- meteo_all_stations(Sys.Date() - 1)"),
  chunk('cat("Relevés du ", format(Sys.Date() - 1, "%A %d %B %Y"), " pour ", nrow(d), " stations Météo-France.\\n\\n", meteo_extremes_md(d), sep = "")', asis = TRUE),
  "\n# Toutes les stations\n",
  chunk('meteo_reactable(d, page_size = 25, columns = list(
  Dépt = reactable::colDef(maxWidth = 60),
  Station = reactable::colDef(minWidth = 130)))'),
  footer))

# --- station_list.qmd : liste Météo-France des stations temps réel -------------
write_qmd("station_list", paste0(
  yaml("Météo — Liste des stations Météo-France"),
  setup('load(here::here("data", "meteo", "station_list.RData"))
sl <- station_list |>
  dplyr::transmute(
    Station = stringr::str_to_title(.data[[names(station_list)[4]]]),
    Dépt = substr(`NUM_POSTE / NUMER_STA`, 1, 2),
    `Id OMM` = Id_OMM, `N° poste` = `NUM_POSTE / NUMER_STA`,
    `Altitude m` = as.numeric(`ALTITUDE / ELEVATION`),
    Ouverture = as.Date(`DATE OUVERTURE / DATA SERIES START`, "%Y%m%d"),
    Type = `TYPE POSTE / STATION TYPE`
  ) |>
  dplyr::arrange(Dépt, Station)'),
  '
Liste des stations temps réel publiée par Météo-France (juillet 2014, `station_list.R`). L\'identifiant meteo60 d\'une station est son **Id OMM** s\'il existe, sinon son **N° poste** (ex. Samoëns : 74258002).

**Type de poste**

- **0** : stations synoptiques Radome, temps réel, expertise à J+1
- **1** : stations Radome du Réseau Régional Auto (RRA), temps réel, expertise à J+1
- **2** : stations automatiques non Radome, temps réel, expertise à J+1
- **3** : stations automatiques non Radome, temps réel, expertise à J+2

# Stations OMM
',
  chunk('meteo_reactable(dplyr::filter(sl, !is.na(`Id OMM`)), page_size = 25)'),
  "\n# Toutes les stations\n",
  chunk("meteo_reactable(sl, page_size = 25)"),
  footer))

# --- redirections ----------------------------------------------------------------
purrr::iwalk(redirects, \(to, from) write_qmd(from, sprintf('---
title: "Météo"
include-in-header:
  text: \'<meta http-equiv="refresh" content="0; url=%s"><link rel="canonical" href="https://fgeerolf.com/data/meteo/%s">\'
---

Cette page a déménagé : [%s](%s).
', to, to, to, to)))
