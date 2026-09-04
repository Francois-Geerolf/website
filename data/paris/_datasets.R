#!/usr/bin/env Rscript
# Rafraîchit les jeux de données opendata.paris.fr utilisés par le site, au
# format Parquet (export natif de l'API Opendatasoft Explore v2.1), et les
# écrit dans data/paris/<id>.parquet. Ce dossier vit uniquement dans
# ~/iCloud (pas dans le dépôt git) : à relancer à la main, pas de CI.
#
#   cd ~/iCloud/website/data/paris && Rscript _catalogue.R   # d'abord
#   cd ~/iCloud/website/data/paris && Rscript _datasets.R
#
# La liste des jeux à récupérer n'est PAS codée en dur : c'est
#   { basename(data/paris/<id>.qmd) } ∪ { bases de carte }
# restreint aux identifiants présents dans data/paris/catalogue.csv (produit
# par _catalogue.R). Ajouter une page data/paris/<id>.qmd suffit donc à faire
# récupérer data/paris/<id>.parquet au prochain passage.

suppressMessages({library(dplyr); library(arrow)})

dir <- here::here("data", "paris")
base_api <- "https://opendata.paris.fr/api/explore/v2.1/catalog/datasets"

# --- Catalogue = source de vérité des identifiants + métadonnées -------------
catalogue <- readr::read_csv(file.path(dir, "catalogue.csv"), show_col_types = FALSE)

# --- Options par jeu (uniquement les exceptions) ----------------------------
# year_col   : ne garder que le dernier millésime de cette colonne
# simplify_m : tolérance de simplification de geo_shape (m, Lambert 93)
# buffer_m   : buffer aller-retour avant simplification (referme les rues
#              découpées des secteurs scolaires)
opts <- tibble::tribble(
  ~id,                                       ~year_col,     ~simplify_m, ~buffer_m,
  "secteurs-scolaires-maternelles",          "annee_scol",  20,          25,
  "secteurs-scolaires-ecoles-elementaires",  "annee_scol",  20,          25,
  "secteurs-scolaires-colleges",             "annee_scol",  20,          25,
)

# --- Jeux à récupérer : pages existantes + fonds de carte -------------------
pages   <- setdiff(tools::file_path_sans_ext(list.files(dir, pattern = "\\.qmd$")),
                   c("index", "catalogue"))
fonds   <- c("arrondissements")            # utilisé comme calque, sans page dédiée
targets <- union(pages, fonds)

inconnus <- setdiff(targets, catalogue$id)
if (length(inconnus))
  message("⚠ absents du catalogue, ignorés : ", paste(inconnus, collapse = ", "))
targets <- intersect(targets, catalogue$id)

# geo_shape est du WKB (GeoParquet). Allègement : WKB -> sfc -> Lambert 93 ->
# (buffer aller-retour optionnel) -> st_simplify -> WGS84 -> WKB.
simplify_geo <- function(wkb_list, tol_m, buffer_m = 0) {
  g <- sf::st_as_sfc(structure(lapply(wkb_list, as.raw), class = "WKB"), EWKB = FALSE)
  sf::st_crs(g) <- 4326
  g <- sf::st_transform(g, 2154)
  if (buffer_m > 0) g <- sf::st_buffer(sf::st_buffer(g, buffer_m), -buffer_m * 0.7)
  g <- sf::st_simplify(g, dTolerance = tol_m, preserveTopology = TRUE)
  g <- sf::st_make_valid(sf::st_transform(g, 4326))
  lapply(sf::st_as_binary(g, EWKB = FALSE), as.raw)
}

for (id in targets) {
  meta <- dplyr::filter(catalogue, id == !!id)
  o    <- dplyr::filter(opts, id == !!id)
  url  <- sprintf("%s/%s/exports/parquet?lang=fr&timezone=Europe%%2FParis", base_api, id)
  dest <- file.path(dir, paste0(id, ".parquet"))

  tmp <- tempfile(fileext = ".parquet")
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  d <- arrow::read_parquet(tmp)
  n0 <- nrow(d)

  if (nrow(o) == 1) {
    if (!is.na(o$year_col) && o$year_col %in% names(d))
      d <- dplyr::filter(d, .data[[o$year_col]] == max(.data[[o$year_col]], na.rm = TRUE))
    if (!is.na(o$simplify_m) && "geo_shape" %in% names(d))
      d$geo_shape <- simplify_geo(d$geo_shape, o$simplify_m, dplyr::coalesce(o$buffer_m, 0))
  }

  arrow::write_parquet(d, dest, compression = "zstd")
  message(sprintf("%-58s %6d -> %5d l.  %5.0f ko  (catalogue : %s l., maj %s)",
                  id, n0, nrow(d), file.size(dest) / 1024,
                  format(meta$lignes, big.mark = " "), meta$modifie))
}
