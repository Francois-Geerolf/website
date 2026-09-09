# Load packages --------

source(here::here("_packages.R"))

load(here::here("data", "_datasets.RData"))
load(here::here("data", "_themes.RData"))

# # Install any missing packages
# installed <- packages %in% rownames(installed.packages())
# if (any(!installed)) {
#   install.packages(packages[!installed], dependencies = TRUE)
# }

# Load all packages# Load all packages silently
invisible(
  suppressMessages(
    suppressWarnings(
      lapply(packages, library, character.only = TRUE)
    )
  )
)

# Echo = T
knitr::opts_chunk$set(echo = T)

# functions -----

ig_r <-function(paper, file) i_g(paste0("replications/", paper, "_files/figure-html/", file, "-1.png"))


# Petit logo d'une source de données, à accoler devant son nom (p. ex.
# "insee" -> <img insee.png> insee). Renvoie une chaîne HTML `<img>` quand
# data/logos/<source>.png existe, "" sinon. URL absolue -> fonctionne quelle
# que soit la page qui l'affiche (ig_d() sur data/<src>/<ds>.qmd, tables de
# data/index.qmd, pages de thèmes...). Vectorisé.
#
# Les PNG des logos vivent dans ~/iCloud/website/data/logos/ (publiés par
# rsync), PAS dans le dépôt git -- on ne peut donc pas se fier à file.exists()
# côté CI. Seul le manifeste data/logos/logos.txt (un identifiant par ligne,
# régénéré par data/_logos_download.sh) est versionné : c'est lui qui dit à
# la CI quels logos existent. On garde en plus un file.exists() local pour
# les rendus faits directement dans ~/iCloud (manifeste éventuellement en
# retard).
source_logo_md <- function(source) {
  if (!isTRUE(knitr::is_html_output())) return(rep("", length(source)))
  mf    <- here::here("data", "logos", "logos.txt")
  avail <- if (file.exists(mf)) trimws(readLines(mf, warn = FALSE)) else character()
  f     <- here::here("data", "logos", paste0(source, ".png"))
  ifelse(
    source %in% avail | file.exists(f),
    sprintf(paste0('<img src="https://fgeerolf.com/data/logos/%s.png" alt="" ',
                   'style="height:1.1em;width:1.1em;object-fit:contain;',
                   'vertical-align:-0.2em;margin-right:.4em">'),
            source),
    ""
  )
}

# Marqueur des pages de thèmes dans les tables du catalogue (data/index.qmd,
# data/themes.qmd). Un thème regroupe des graphiques issus de plusieurs
# sources : il n'a pas de logo d'organisme. Une icône "pile de couches"
# grise, à la même taille 1.1em que source_logo_md(), le signale comme
# thème sans faire concurrence aux logos colorés des sources. HTML
# seulement ; Font Awesome est chargé site-wide via data/_common.yml.
theme_icon_md <- function(theme = character()) {
  if (!isTRUE(knitr::is_html_output())) return(rep("", length(theme)))
  rep(paste0('<i class="fas fa-layer-group" aria-hidden="true" title="Thème" ',
             'style="font-size:1.1em;color:#888780;opacity:.75;',
             'margin-right:.45em;vertical-align:-0.05em"></i>'),
      length(theme))
}


ig_d <- function(source, dataset, file){
  path_base <- paste0("data/", source, "/", dataset, "_files/figure-html/", file, "-1")
  img_path <- paste0(path_base, ".png")

  # The chart shown below is a static PNG baked in at the dataset's last
  # render -- it's only as fresh as the OLDER of "when the underlying data
  # was last downloaded" and "when the page was last rendered" (e.g. new
  # data can arrive after the last render, in which case the picture is
  # stale even though the data itself isn't). min() of the two, from
  # `datasets` (loaded from _datasets.RData at the top of this file),
  # captures that conservatively; NA if the dataset isn't in the catalog.
  # .data$/.env$ disambiguate the filter columns from this function's own
  # source/dataset argument names, which are identical to them.
  freshness <- datasets %>%
    dplyr::filter(.data$source == .env$source, .data$dataset == .env$dataset)
  updated <- if (nrow(freshness) == 1) {
    format(pmin(as.Date(freshness$data_updated), as.Date(freshness$.html), na.rm = TRUE), "%d %b %Y")
  } else {
    NA_character_
  }

  # Source/Dataset link to this site's own pages -- same URL convention as
  # source_dataset_file_updates() above (https://fgeerolf.com/data/<source>
  # and .../<source>/<dataset>.html) -- which works for every source in
  # this codebase, unlike an upstream-database link (only verified for
  # eurostat's databrowser, dropped here in favor of this fully general
  # pattern).
  links <- tibble::tibble(
    Source = paste0(source_logo_md(source),
                    "[", source, "](https://fgeerolf.com/data/", source, ")"),
    Dataset = paste0("[", dataset, "](https://fgeerolf.com/data/", source, "/", dataset, ".html)"),
    Updated = updated,
    PNG = paste0("[png](https://fgeerolf.com/", path_base, ".png)"),
    PDF = paste0("[pdf](https://fgeerolf.com/", path_base, ".pdf)")
  ) %>%
    gt::gt() %>%
    gt::fmt_markdown(columns = everything()) %>%
    gt::cols_align(align = "center", columns = everything()) %>%
    gt::tab_options(column_labels.font.weight = "bold")

  if (knitr::is_html_output()) {
    # print()-ing a gt table as a side effect (rather than returning it as
    # the chunk's last value) skips knitr's special knit_print handling for
    # recognized table/graphics classes: gt's print.gt_tbl method just
    # writes its raw HTML to the console as plain text in a non-interactive
    # render, which knitr then captures like any other printed output and
    # HTML-escapes (`<table>` shows up as literal `&lt;table&gt;` on the
    # page). knitr::asis_output() bypasses that by inserting its argument
    # verbatim regardless of the calling chunk's own results option, so we
    # build the table's raw HTML (gt::as_raw_html()) and the image tag
    # ourselves and hand the combination to it directly.
    img_tag <- sprintf('<p><img src="https://fgeerolf.com/%s" class="img-fluid figure-img"></p>', img_path)
    return(knitr::asis_output(paste0(gt::as_raw_html(links), img_tag)))
  }

  # Interactive preview / LaTeX-PDF output: no asis-output escaping issue
  # there (gt renders in RStudio's viewer; the table is a secondary detail
  # in a PDF), so keep the simple print()-then-graphic behavior.
  print(links)
  i_g(img_path)
}

i_g <- function(path) {
  new_path <- gsub("https://fgeerolf.com/", "", path)
  new_path_pdf <- paste0(gsub(".png", "", new_path), ".pdf")
  if (interactive()) {
    if (file.exists(paste0("~/iCloud/website/", new_path))) {
      return(knitr::include_graphics(paste0("~/iCloud/website/", new_path)))
    } else{
      return(knitr::include_graphics(paste0("~/iCloud/", new_path)))
    }
  } else if (knitr::is_latex_output()) {
    if (file.exists(paste0("~/iCloud/website/", new_path_pdf))) {
      return(knitr::include_graphics(paste0("~/iCloud/website/", new_path_pdf)))
    } else if (file.exists(paste0("~/iCloud/website/", new_path))){
      return(knitr::include_graphics(paste0("~/iCloud/website/", new_path)))
    } else {
      return(knitr::include_graphics(paste0("~/iCloud/", new_path)))
    }
  }
  else {
    return(knitr::include_graphics(paste0("https://fgeerolf.com/", new_path)))
  }
}

ig_b <- function(source = "", folder = "", folder2 = "", folder3 = "", file = "", ext = ".png") {
  # Build path components conditionally
  parts <- c("bib", source, folder, folder2, folder3, file)
  # Remove empty strings
  path <- paste(parts[parts != ""], collapse = "/")
  # Add file extension only if not already present
  if (!grepl(paste0("\\", ext, "$"), path)) {
    path <- paste0(path, ext)
  }
  i_g(path)
}

# --- Cachet "derniere compilation" pour les pages de themes -------------
# Pendant de dataset_info_lines() pour les pages data/*.qmd : elles
# agregent des dizaines de graphiques pre-rendus via ig_d() et ne possedent
# aucun parquet propre, donc "Last data update" n'a pas de sens ici -- seul
# compte le moment ou la page elle-meme a ete reconstruite. Memes mecaniques
# de pied de page que dataset_info_lines(position = "footer") : appeler
# depuis un chunk `output: asis` (n'importe ou -- un petit script inline
# deplace le bloc en fin de #quarto-document-content au chargement). Style
# .dataset-info-footer dans data/_fgeerolf.scss. GARDER SYNCHRO entre
# ~/github/website/_rinit.R et ~/Dropbox/website/_rinit.R.
page_compile_line <- function() {
  cat("::: {.dataset-info .dataset-info-footer}\n\n",
      "**Last compile**: ", format(Sys.time(), "%d %b %Y, %H:%M"),
      "\n\n:::\n\n",
      "```{=html}\n",
      "<script>\n",
      "(function(){\n",
      "  function move(){\n",
      "    var note = document.querySelector('.dataset-info-footer');\n",
      "    var main = document.getElementById('quarto-document-content');\n",
      "    if (!note || !main) return;\n",
      "    var cell = note.closest('.cell, .column-container');\n",
      "    main.appendChild(note);\n",
      "    if (cell && cell !== main && !cell.querySelector('*:not(script)')) cell.remove();\n",
      "  }\n",
      "  if (document.readyState === 'loading')\n",
      "    document.addEventListener('DOMContentLoaded', move);\n",
      "  else move();\n",
      "})();\n",
      "</script>\n",
      "```\n",
      sep = "")
  invisible(NULL)
}

add_flags <- ggimage::geom_image(
  data = function(df) {
    # Guard empty/invalid
    if (is.null(df) || nrow(df) == 0) return(df[0, , drop = FALSE])
    df <- tibble::as_tibble(df) %>% dplyr::ungroup()
    
    # --- Find the y/value column (first match wins)
    y_candidates <- c("obsValue", "OBS_VALUE", "values", "value")
    y_col <- y_candidates[y_candidates %in% names(df)][1]
    
    # --- Find the area column (Ref_area, Geo, country, etc.)
    area_candidates <- c(
      "Ref_area","ref_area",
      "Geo","geo","GEO",
      "country","Country","COUNTRY", "Location"
    )
    area_col <- area_candidates[area_candidates %in% names(df)][1]
    
    # Need: date, area, and a y column
    if (is.na(y_col) || is.na(area_col) || !"date" %in% names(df))
      return(df[0, , drop = FALSE])
    
    # Standardize to .y__ and .area__
    df <- df %>%
      dplyr::mutate(
        .y__ = .data[[y_col]],
        .area__ = as.character(.data[[area_col]])
      )
    
    # Largest group size across dates
    n_max <- max(dplyr::count(df, date, name = "n")$n, na.rm = TRUE)
    if (!is.finite(n_max) || n_max < 1) return(df[0, , drop = FALSE])
    
    # Pick the date with the best separation in y (to avoid overlaps)
    per_date <- df %>%
      dplyr::group_by(date) %>%
      dplyr::filter(dplyr::n() == n_max) %>%
      dplyr::arrange(.y__, .by_group = TRUE) %>%
      dplyr::summarise(
        dist = if (n_max > 1) min(diff(.y__), na.rm = TRUE) else Inf,
        .groups = "drop"
      )
    if (nrow(per_date) == 0) return(df[0, , drop = FALSE])
    
    best_date <- per_date %>%
      dplyr::arrange(dplyr::desc(dist), date) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::pull(date)
    
    # Helper: slugify area to file name
    slugify <- function(x) {
      # transliterate accents to ASCII where possible
      x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
      x <- tolower(x)
      x <- gsub("'", "", x)                 # drop apostrophes
      x <- gsub("[^a-z0-9]+", "-", x)       # non-alnum -> hyphen
      x <- gsub("^-+|-+$", "", x)           # trim hyphens
      x
    }
    
    # Keep rows from that best date and build image path from area
    df %>%
      dplyr::filter(date %in% best_date) %>%
      dplyr::arrange(.y__) %>%
      dplyr::mutate(
        image = file.path(
          "../../icon/flag/round",
          paste0(slugify(.area__), ".png")
        )
      )
  },
  mapping = ggplot2::aes(x = date, y = .y__, image = image),
  asp = 1.5,
  inherit.aes = FALSE
)

print_table_long <- . %>%
  gt::gt() %>%
  gt::fmt_markdown(., columns = one_of(c("source", "dataset", "theme"))) %>%
  gt::cols_align(align = "center", columns = everything()) %>% 
  gt::tab_options(column_labels.font.weight = "bold")

print_table <- . %>%
  knitr::kable(align = "c", booktabs = T, linesep = "", longtable = T, escape = F) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            latex_options = c("striped", "hold_position", "repeat_header"))

print_table2 <- . %>%
  gt::gt() %>%
  gt::cols_align(align = "center", columns = everything()) %>% 
  gt::fmt_markdown(., columns = one_of(c("source", "dataset", "theme", "id"))) %>%
  gt::tab_options(column_labels.font.weight = "bold")


print_table_no_escape <- . %>%
  knitr::kable(align = "c", booktabs = T, linesep = "", longtable = T, escape = F) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            latex_options = c("striped", "hold_position", "repeat_header"))


print_table_noname <- . %>%
  knitr::kable(align = "c", booktabs = T, linesep = "", longtable = T, col.names = NULL) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            latex_options = c("striped", "hold_position", "repeat_header"))

# (source, dataset) tracés sur une page de thème, extraits de ses appels
# ig_d() par data/_themes.R et stockés (au format long, avec une colonne
# `theme`) dans data/_themes.RData sous le nom `theme_datasets`. Remplace le
# `<theme> <- tribble(~source, ~dataset, ...)` qui était maintenu à la main
# en tête de chaque data/<theme>.qmd. Se branche directement sur
# source_dataset_file_updates().
theme_source_datasets <- function(theme) {
  theme_datasets %>%
    dplyr::filter(.data$theme == .env$theme) %>%
    dplyr::distinct(source, dataset) %>%
    dplyr::arrange(source, dataset)
}

# Joins in just Title + Updated (single pmin(data_updated, .html) date,
# same as ig_d()'s Updated above) -- not the full raw catalog
# (Nobs/qmd_duration_sec/qmd_rendered_at etc), which is what this used to
# left_join() wholesale against the global `datasets` loaded at the top of
# this file.
source_dataset_file_updates <- . %>%
  left_join(
    datasets %>%
      dplyr::transmute(source, dataset, Title,
                        Updated = pmin(as.Date(data_updated), as.Date(.html), na.rm = TRUE)),
    by = c("source", "dataset")
  ) %>%
  mutate(dataset = glue::glue("[{dataset}](https://fgeerolf.com/data/{source}/{dataset}.html)"),
         dataset = map(dataset, gt::md),
         source = glue::glue("{source_logo_md(source)}[{source}](https://fgeerolf.com/data/{source})"),
         source = map(source, gt::md)) %>%
  gt::gt() %>%
  gt::cols_align(align = "center", columns = everything()) %>%
  gt::tab_options(column_labels.font.weight = "bold")


source_dataset_file_updates2 <- . %>%
  mutate(dataset = glue::glue("[{dataset}](https://fgeerolf.com/data/{source}/{dataset}.html)"),
         dataset = map(dataset, gt::md)) %>%
  select(-source) %>%
  gt::gt() %>%
  gt::cols_align(align = "center", columns = everything()) %>% 
  gt::tab_options(column_labels.font.weight = "bold")


source_dataset_file_updates3 <- . %>%
  mutate(dataset = glue::glue("[{dataset}](https://fgeerolf.com/data/{source}/{dataset}.html)"),
         source = glue::glue("{source_logo_md(source)}[{source}](https://fgeerolf.com/data/{source})")) %>%
  gt::gt() %>%
  fmt_markdown(columns = c("dataset", "source")) %>%
  gt::cols_align(align = "center", columns = everything()) %>% 
  gt::tab_options(column_labels.font.weight = "bold") %>% 
  # Interactive gt
  gt::opt_interactive(use_search = T,
                      use_pagination = T,
                      use_pagination_info = F,
                      use_compact_mode = T,
                      use_resizers = TRUE,
                      page_size_default = 20,
                      page_size_values = c(5, 10, 15, 20))


# Single Updated column (pmin of the .RData/.html file-mtime scan) instead
# of two separate date columns.
source_dataset_title_file_updates <- . %>%
  mutate(type = map(source, ~ tibble(type = c(".RData", ".html")))) %>%
  unnest %>%
  mutate(date = paste0("~/iCloud/website/data/", source, "/", dataset, type) %>% file.info %>% pluck("mtime") %>% as.Date()) %>%
  spread(type, date) %>%
  mutate(Updated = pmin(`.RData`, `.html`, na.rm = TRUE)) %>%
  select(-`.RData`, -`.html`) %>%
  mutate(Title = read_lines(paste0("~/iCloud/website/data/", source, "/",dataset, ".qmd"), skip = 1, n_max = 1) %>% gsub("title: ", "", .) %>% gsub("\"", "", .)) %>%
  select(Title, everything()) %>%
  mutate(dataset = glue::glue("[{dataset}](https://fgeerolf.com/data/{source}/{dataset}.html)"),
         dataset = map(dataset, gt::md),
         source = glue::glue("{source_logo_md(source)}[{source}](https://fgeerolf.com/data/{source})"),
         source = map(source, gt::md)) %>%
  gt::gt() %>%
  gt::cols_align(align = "center", columns = everything()) %>%
  gt::tab_options(column_labels.font.weight = "bold")

theme_file_updates <- . %>%
  left_join(themes, by = c("theme")) %>%
  # Le nom du thème EST le lien (vers sa page) -- plus de colonne "Link"
  # séparée avec un libellé "Link" peu parlant. Icône "pile de couches"
  # devant, comme dans les tables du catalogue (theme_icon_md()).
  mutate(theme = glue::glue(
    "{theme_icon_md(theme)}[{theme}](https://fgeerolf.com/data/{theme}.html)")) %>%
  dplyr::select(-dplyr::any_of("Link")) %>%
  gt::gt() %>%
  gt::fmt_markdown(columns = "theme") %>%
  gt::cols_align(align = "center", columns = everything()) %>%
  gt::tab_options(column_labels.font.weight = "bold")



print_table_conditional <- function(data){
  if (dim(data)[1] > 30){
    if (knitr::is_html_output()) DT::datatable(data, filter = 'top', rownames = F) else data
  } else{
    if (knitr::is_html_output()) print_table(data) else data
  }
}

print_table_conditional2 <- function(data){
  if (dim(data)[1] > 30){
    if (knitr::is_html_output()) DT::datatable(data, filter = 'top', rownames = F) else data
  } else{
    if (knitr::is_html_output()) print_table2(data) else data
  }
}



print_table_conditional_10 <- function(data){
  if (dim(data)[1] > 10){
    if (knitr::is_html_output()) DT::datatable(data, filter = 'top', rownames = F) else data
  } else{
    if (knitr::is_html_output()) print_table(data) else data
  }
}

print_table_conditional_20 <- function(data){
  if (dim(data)[1] > 20){
    if (knitr::is_html_output()) DT::datatable(data, filter = 'top', rownames = F) else data
  } else{
    if (knitr::is_html_output()) print_table(data) else data
  }
}

print_table_conditional_30 <- function(data){
  if (dim(data)[1] > 30){
    if (knitr::is_html_output()) DT::datatable(data, filter = 'top', rownames = F) else data
  } else{
    if (knitr::is_html_output()) print_table(data) else data
  }
}

print_table_conditional_100 <- function(data){
  if (dim(data)[1] > 100){
    if (knitr::is_html_output()) DT::datatable(data, filter = 'top', rownames = F) else data
  } else{
    if (knitr::is_html_output()) print_table(data) else data
  }
}



metadata_load <- function(code, CL_code, data = QNA_EXPENDITURE_CAPITA_var){
  assign(code, as.data.frame(data@codelists, codelistId = CL_code) %>%
           select(id, label.en) %>%
           setNames(c(code, str_to_title(code))),
         envir = .GlobalEnv)
}

metadata_load_fr <- function(code, CL_code, data = QNA_EXPENDITURE_CAPITA_var){
  assign(code, as.data.frame(data@codelists, codelistId = CL_code) %>%
           select(id, label.fr) %>%
           setNames(c(code, str_to_title(code))),
         envir = .GlobalEnv)
}

code_names <- function(data = QNA_EXPENDITURE_CAPITA_var){
  assign("code_names",
         data@codelists@codelists %>%
           vapply(., function(x) x@id, "character") %>%
           as_tibble %>%
           arrange(value) %>%
           mutate(empty = 1) %>%
           spread(value, empty),
         envir = .GlobalEnv)
}



code_names_unique <- function(data = QNA_EXPENDITURE_CAPITA_var){
  assign("code_names_unique",
         data %>%
           select(-obsTime, -obsValue) %>%
           select_if(~ n_distinct(.) > 1) %>%
           names(.) %>%
           as_tibble %>%
           arrange(value) %>%
           mutate(empty = 1) %>%
           spread(value, empty),
         envir = .GlobalEnv)
}


obsTime_FREQUENCY_to_date <- function(obsTime, FREQUENCY){
  if (FREQUENCY == "A"){
    date = paste0(obsTime, "-01-01") %>% as.Date
  } else if (FREQUENCY == "Q"){
    year = obsTime %>% substr(1, 4)
    qtr = obsTime %>% substr(7, 7) %>% as.numeric
    month = (qtr - 1)*3 + 1
    month = month %>% str_pad(., 2, pad = "0")
    date = paste0(year, "-", month, "-01") %>% as.Date
  } else if (FREQUENCY == "M"){
    date = paste0(obsTime, "-01-01") %>% as.Date
  }
  return(date)
}

frequency_to_date <- function(data){
  data %>%
    rowwise() %>%
    mutate(date = obsTime_FREQUENCY_to_date(obsTime, FREQUENCY)) %>%
    select(-obsTime) %>%
    select(date, everything())
}

quarter_to_date <- function(data){
  data %>%
    mutate(year = obsTime %>% substr(1, 4),
           qtr = obsTime %>% substr(7, 7) %>% as.numeric,
           month = (qtr - 1)*3 + 1,
           month = month %>% str_pad(., 2, pad = "0"),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -qtr, -month, -obsTime) %>%
    select(date, everything())
}


quarter_to_enddate <- function(data){
  data %>%
    mutate(year = obsTime %>% substr(1, 4),
           qtr = obsTime %>% substr(7, 7) %>% as.numeric,
           month = (qtr)*3,
           month = month %>% str_pad(., 2, pad = "0"),
           date = as.Date(paste0(year, "-", month, "-01")),
           date = date + months(1) - days(1)) %>%
    select(-year, -qtr, -month, -obsTime) %>%
    select(date, everything())
}



quarter_to_date2 <- function(data){
  data %>%
    mutate(year = yearqtr %>% substr(1, 4),
           qtr = yearqtr %>% substr(6, 6) %>% as.numeric,
           month = (qtr - 1)*3 + 1,
           month = month %>% str_pad(., 2, pad = "0"),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -qtr, -month, -yearqtr) %>%
    select(date, everything())
}


month_to_date <- function(data){
  data %>%
    mutate(date = paste0(obsTime, "-01") %>% as.Date) %>%
    select(-obsTime) %>%
    select(date, everything())
}


year_to_date <- function(data){
  data %>%
    mutate(date = paste0(obsTime, "-01-01") %>% as.Date) %>%
    select(-obsTime) %>%
    select(date, everything())
}


year_to_enddate <- function(data){
  data %>%
    mutate(date = paste0(obsTime, "-12-31") %>% as.Date) %>%
    select(-obsTime) %>%
    select(date, everything())
}

yearqtr_to_date <- function(data){
  data %>%
    mutate(year = yearqtr %>% floor,
           month = (yearqtr - year)*12 + 1,
           month = str_pad(month, 2, pad = 0),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -month, -yearqtr) %>%
    select(date, everything())
}

yearqtr_to_enddate <- function(data){
  data %>%
    mutate(year = yearqtr %>% floor,
           month = (yearqtr - year)*12 + 1,
           month = str_pad(month, 2, pad = 0),
           date = as.Date(paste0(year, "-", month, "-01")) + months(1) - day(1)) %>%
    select(-year, -month, -yearqtr) %>%
    select(date, everything())
}



yearqtr_to_date2 <- function(data){
  data %>%
    mutate(year = yearqtr %>% floor,
           month = (yearqtr - year)*12 + 1,
           month = str_pad(month, 2, pad = 0),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -month) %>%
    select(date, everything())
}

