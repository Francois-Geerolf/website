# Catalogue des pages de thèmes (`themes`) + datasets tracés sur chacune
# (`theme_datasets`), écrit dans ~/github/website/data/_themes.RData
# (versionné, source unique de vérité, chargé par _rinit.R des deux côtés).
#
# Comme data/_datasets.R : versionné dans github mais partie du tooling de
# build local. Symlinké depuis ~/iCloud/website/data/_themes.R ; le setwd
# ci-dessous le rend lançable de n'importe où. À lancer AVANT _datasets.R
# pour que la régénération SEO de _datasets.R reparte d'un _themes.RData frais.
#
# Pourquoi lire les deux arbres : 10 pages de thème qui scrapent des sites
# au rendu (investing.com, CME...) sont bloquées depuis GitHub Actions ;
# elles ne vivent QUE dans ~/iCloud/website/data et sont rendues localement.
# Elles doivent quand même figurer au catalogue (themes.qmd, sitemap,
# recherche), d'où l'union ci-dessous.
setwd(path.expand("~/github/website/data"))
source(here::here("_rinit.R"))

icloud_data <- path.expand("~/iCloud/website/data")
if (!dir.exists(icloud_data))
  warning("Dossier iCloud introuvable (", icloud_data,
          ") -- le catalogue de thèmes sera incomplet.")

collect_qmd <- function(dir) {
  if (!dir.exists(dir))
    return(tibble::tibble(theme = character(), path = character()))
  f <- list.files(dir, pattern = "\\.qmd$", full.names = TRUE, ignore.case = TRUE)
  tibble::tibble(theme = tools::file_path_sans_ext(basename(f)), path = f)
}

# iCloud d'abord -> distinct() garde iCloud quand un thème est dans les deux
# arbres (il en est la source de rendu de référence).
qmd <- dplyr::bind_rows(
  collect_qmd(icloud_data),
  collect_qmd(path.expand("~/github/website/data"))
) |>
  dplyr::filter(
    !theme %in% c("api", "index", "themes"),
    !grepl("_update", theme, perl = TRUE)
  ) |>
  dplyr::distinct(theme, .keep_all = TRUE) |>
  dplyr::arrange(theme)

first_title <- function(path, theme) {
  ln <- read_lines(path, skip = 1, n_max = 1)
  t  <- ln %>% gsub("title: ", "", .) %>% gsub("\"", "", .)
  # Les copies de travail iCloud suffixent parfois le titre de " - <slug>"
  # (repère d'onglet d'éditeur) -- on le retire pour le catalogue.
  sub(paste0("\\s*-\\s*\\Q", theme, "\\E\\s*$"), "", t, perl = TRUE)
}

# `themes` = la colonne vertébrale : un thème = une page qmd + son titre.
# Ne se dérive PAS de `theme_datasets` : (1) le Title vient du YAML du qmd,
# (2) ~9 pages (datasets, inegalites, investment, meteo, niveau-de-vie,
# pauvrete, productivite, ...) n'ont aucun ig_d() donc sont absentes de
# `theme_datasets`. L'URL de la page est `.../data/{theme}.html` (aucune
# colonne Link stockée : elle n'était lue nulle part).
themes <- qmd |>
  dplyr::transmute(theme, Title = purrr::map2_chr(path, theme, first_title))

# --- (source, dataset, file) tracés sur chaque page, extraits des appels
# ig_d("source", "dataset", "file"). Remplace le tribble
# `<theme> <- tribble(~source, ~dataset, ...)` maintenu à la main en tête de
# chaque page : `theme_datasets` est le tibble unique regroupant toutes ces
# listes, avec une colonne `theme`. Consommé via
# theme_source_datasets("<theme>") (défini dans _rinit.R).
extract_ig_d <- function(path) {
  txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  m <- stringr::str_match_all(
    txt,
    'ig_d\\(\\s*"([^"]+)"\\s*,\\s*"([^"]+)"\\s*,\\s*"([^"]+)"\\s*\\)'
  )[[1]]
  if (nrow(m) == 0L)
    return(tibble::tibble(source = character(), dataset = character(), file = character()))
  tibble::tibble(source = m[, 2], dataset = m[, 3], file = m[, 4])
}

theme_datasets <- purrr::pmap_dfr(
  qmd[c("theme", "path")],
  function(theme, path) dplyr::mutate(extract_ig_d(path), theme = theme)
) |>
  dplyr::distinct(theme, source, dataset, file) |>
  dplyr::select(theme, source, dataset, file) |>
  dplyr::arrange(theme, source, dataset, file)

save(themes, theme_datasets, file = "_themes.RData")

# Comme _datasets.R : on stage le résultat pour que le `git commit; git push`
# des scripts de build le récupère (aucun _build*.sh ne l'ajoute).
try(system2("git", c("-C", shQuote(here::here()), "add", "--", "data/_themes.RData")))

print(themes)
print(theme_datasets)
