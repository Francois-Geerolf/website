# Audience des pages /data/ -> data/_pageviews.RData
# ---------------------------------------------------------------------------
# Source principale : GoatCounter (API). Source d'appoint optionnelle : logs
# d'accès serveur archivés (répertoire ~/iCloud/website/data/_status/access_logs).
#
# Sortie : objet `pageviews` (une ligne par (source, dataset)) :
#   source, dataset, path,
#   visitors_90d, pageviews_90d,      <- fenêtre glissante 90 jours
#   visitors_365d, pageviews_365d,
#   pageviews_all,                    <- tout l'historique dispo
#   last_seen,
#   rank_score                        <- pour trier (voir plus bas)
# + `pageviews_source` : total par source.
#
# CONFIG (une des deux) :
#   - variables d'env GOATCOUNTER_SITE (= "fgeerolf") et GOATCOUNTER_TOKEN
#   - ou, dans ~/iCloud/passwords/keys.R :
#       goatcounter_site  <- "fgeerolf"
#       goatcounter_token <- "xxxxxxxx"
#
# Si rien n'est configuré / l'API échoue : on écrit une table vide et on
# sort proprement (data/_datasets.R et data/index.qmd continuent de tourner).
# ---------------------------------------------------------------------------

suppressWarnings(suppressMessages({
  library(tidyverse); library(lubridate)
}))

setwd(here::here("data"))
out_file <- "_pageviews.RData"

empty_pageviews <- function(msg = NULL) {
  if (!is.null(msg)) message("[_pageviews] ", msg, " -> table vide")
  pageviews <- tibble(
    source = character(), dataset = character(), path = character(),
    visitors_90d = integer(), pageviews_90d = integer(),
    visitors_365d = integer(), pageviews_365d = integer(),
    pageviews_all = integer(), last_seen = as.Date(character()),
    rank_score = double()
  )
  pageviews_source <- tibble(
    source = character(), visitors_90d = integer(), pageviews_90d = integer(),
    visitors_365d = integer(), pageviews_365d = integer(),
    pageviews_all = integer(), rank_score = double()
  )
  save(pageviews, pageviews_source, file = out_file)
  quit(save = "no", status = 0)
}

# --- config -----------------------------------------------------------------
gc_site  <- Sys.getenv("GOATCOUNTER_SITE",  "")
gc_token <- Sys.getenv("GOATCOUNTER_TOKEN", "")
keys <- path.expand("~/iCloud/passwords/keys.R")
if ((!nzchar(gc_site) || !nzchar(gc_token)) && file.exists(keys)) {
  e <- new.env(); try(sys.source(keys, e), silent = TRUE)
  if (!nzchar(gc_site)  && exists("goatcounter_site",  e)) gc_site  <- get("goatcounter_site",  e)
  if (!nzchar(gc_token) && exists("goatcounter_token", e)) gc_token <- get("goatcounter_token", e)
}

# --- GoatCounter API ------------------------------------------------------
# API hébergée (api.json, OpenAPI 2.0) : GET /api/v0/stats/hits
#   query : start / end (datetime RFC 3339, arrondi à l'heure), limit
#   pagination : `exclude_paths` = les path_id déjà reçus, tant que `more`
#   réponse : { hits: [ { path, path_id, count, title, ... } ], more, total }
#   `count` = visiteurs uniques sur la plage (il n'y a pas de `count_unique`
#   séparé dans cette version ; les pages vues par chemin ne sont pas
#   exposées -> on prend `count` pour les deux).
# NB : GoatCounter n'accepte PAS les ':' pourcent-encodés dans start/end
# (httr::GET(query=) encode '%3A' -> l'API renvoie alors 0 hit sans erreur).
# On construit donc la query string à la main, ':' littéral conservé.
gc_hits <- function(start, end) {
  if (!requireNamespace("httr", quietly = TRUE)) return(NULL)
  base <- sprintf("https://%s.goatcounter.com/api/v0/stats/hits", gc_site)
  iso  <- function(d) format(as.POSIXct(paste0(format(as.Date(d)), " 00:00:00"),
                                        tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
  # `end` inclusif : on borne à minuit du lendemain, sinon les visites du
  # jour même (après 00:00 UTC) sont exclues.
  out <- list(); seen <- integer(0); guard <- 0L
  repeat {
    guard <- guard + 1L; if (guard > 50L) break
    qs <- sprintf("start=%s&end=%s&limit=200",
                  iso(start), iso(as.Date(end) + 1))
    if (length(seen)) qs <- paste0(qs, "&exclude_paths=", paste(seen, collapse = ","))
    r <- try(httr::GET(paste0(base, "?", qs),
                       httr::add_headers(Authorization = paste("Bearer", gc_token))),
             silent = TRUE)
    if (inherits(r, "try-error") || httr::status_code(r) != 200) {
      message("[_pageviews] GoatCounter HTTP ",
              if (inherits(r, "try-error")) "erreur" else httr::status_code(r))
      break
    }
    j  <- httr::content(r, "parsed", encoding = "UTF-8")
    st <- j$hits %||% list()
    if (length(st) == 0) break
    out  <- c(out, st)
    seen <- c(seen, map_int(st, ~ as.integer(.x$path_id %||% NA_integer_)))
    seen <- seen[!is.na(seen)]
    if (!isTRUE(j$more)) break
    Sys.sleep(4)   # rate limit GoatCounter (hébergé) : 4 req / 16 s
  }
  if (length(out) == 0) return(tibble(path = character(), count = integer(),
                                      count_unique = integer()))
  tibble(
    path         = map_chr(out, ~ .x$path %||% NA_character_),
    count        = map_dbl(out, ~ as.numeric(.x$count %||% 0)),
    count_unique = map_dbl(out, ~ as.numeric(.x$count %||% 0))
  ) |> filter(!is.na(path))
}

if (!nzchar(gc_site) || !nzchar(gc_token))
  empty_pageviews("GoatCounter non configuré (GOATCOUNTER_SITE / GOATCOUNTER_TOKEN)")

today <- Sys.Date()
h90  <- gc_hits(today - 90,   today)
h365 <- gc_hits(today - 365,  today)
hall <- gc_hits(as.Date("2015-01-01"), today)
if (is.null(h90)) empty_pageviews("package httr absent")

# --- logs serveur archivés (optionnel) ------------------------------------
log_dir <- "_status/access_logs"
log_hits <- NULL
if (dir.exists(log_dir)) {
  lf <- list.files(log_dir, pattern = "\\.log(\\.gz)?$", full.names = TRUE)
  if (length(lf)) {
    lines <- unlist(lapply(lf, function(f)
      tryCatch(readLines(if (grepl("\\.gz$", f)) gzfile(f) else f, warn = FALSE),
               error = function(e) character())))
    # format combiné : IP - - [10/Sep/2026:...] "GET /path HTTP/1.1" 200 ... "UA"
    m <- str_match(lines,
      '^(\\S+) \\S+ \\S+ \\[([^:]+):[^\\]]+\\] "(?:GET|HEAD) (\\S+?)(?:\\?[^ ]*)? HTTP/[0-9.]+" (\\d{3}) \\S+ "[^"]*" "([^"]*)"')
    bots <- "bot|crawl|spider|slurp|bingpreview|facebookexternalhit|semrush|ahrefs|python-requests|curl/|wget|Go-http|node-fetch|HeadlessChrome|Lighthouse"
    logs <- tibble(ip = m[,2], day = dmy(m[,3]), path = m[,4],
                   status = m[,5], ua = m[,6]) |>
      filter(!is.na(path), status == "200",
             str_starts(path, "/data/"),
             !str_detect(coalesce(ua, ""), regex(bots, ignore_case = TRUE)))
    log_hits <- logs |>
      mutate(win90 = day >= today - 90, win365 = day >= today - 365) |>
      group_by(path) |>
      summarise(
        l_pv_90   = sum(win90), l_uv_90 = n_distinct(ip[win90]),
        l_pv_365  = sum(win365), l_uv_365 = n_distinct(ip[win365]),
        l_pv_all  = n(), l_last = max(day, na.rm = TRUE), .groups = "drop"
      )
  }
}

# --- normaliser les chemins vers (source, dataset) -----------------------
# /data/<source>            -> (source, NA)         page d'accueil de la source
# /data/<source>/           ->  idem
# /data/<source>/index.html ->  idem
# /data/<source>/<ds>.html  -> (source, <ds>)
# /data/<source>/<ds>/      -> (source, <ds>)
classify <- function(path) {
  p <- str_replace(path, "^/data/?", "")
  p <- str_replace(p, "/$", "")
  p <- str_replace(p, "\\.html$", "")
  p <- str_replace(p, "\\.pdf$",  "")
  parts <- str_split(p, "/", n = 2)[[1]]
  src <- parts[1]
  ds  <- if (length(parts) < 2 || parts[2] %in% c("", "index")) NA_character_ else parts[2]
  # sous-dossiers <ds>_files : ignore
  if (!is.na(ds) && str_detect(ds, "_files($|/)")) ds <- NA_character_
  tibble(source = src, dataset = ds)
}

combine <- function(hits, pv, uv) {
  if (is.null(hits) || nrow(hits) == 0)
    return(tibble(source = character(), dataset = character(),
                  !!pv := integer(), !!uv := integer()))
  hits |>
    filter(str_starts(path, "/data/")) |>
    bind_cols(map_dfr(hits$path[str_starts(hits$path, "/data/")], classify)) |>
    filter(!is.na(source), source != "") |>
    group_by(source, dataset) |>
    summarise(!!pv := sum(count), !!uv := max(count_unique), .groups = "drop")
}

pv90  <- combine(h90,  "pageviews_90d",  "visitors_90d")
pv365 <- combine(h365, "pageviews_365d", "visitors_365d")
pvall <- combine(hall, "pageviews_all",  "visitors_all") |> select(-visitors_all)

pageviews <- pv90 |>
  full_join(pv365, by = c("source", "dataset")) |>
  full_join(pvall, by = c("source", "dataset")) |>
  mutate(across(where(is.numeric), ~ replace_na(., 0)),
         path = ifelse(is.na(dataset),
                       paste0("/data/", source),
                       paste0("/data/", source, "/", dataset, ".html")),
         last_seen = today)

# fusion logs (max des deux signaux)
if (!is.null(log_hits) && nrow(log_hits)) {
  lg <- log_hits |>
    bind_cols(map_dfr(log_hits$path, classify)) |>
    filter(!is.na(source), source != "") |>
    group_by(source, dataset) |>
    summarise(l_pv_90 = sum(l_pv_90), l_uv_90 = sum(l_uv_90),
              l_pv_365 = sum(l_pv_365), l_uv_365 = sum(l_uv_365),
              l_pv_all = sum(l_pv_all), l_last = max(l_last), .groups = "drop")
  pageviews <- pageviews |>
    full_join(lg, by = c("source", "dataset")) |>
    mutate(
      across(c(pageviews_90d, visitors_90d, pageviews_365d, visitors_365d,
               pageviews_all, l_pv_90, l_uv_90, l_pv_365, l_uv_365, l_pv_all),
             ~ replace_na(., 0)),
      pageviews_90d  = pmax(pageviews_90d,  l_pv_90),
      visitors_90d   = pmax(visitors_90d,   l_uv_90),
      pageviews_365d = pmax(pageviews_365d, l_pv_365),
      visitors_365d  = pmax(visitors_365d,  l_uv_365),
      pageviews_all  = pmax(pageviews_all,  l_pv_all),
      last_seen      = pmax(last_seen, coalesce(l_last, as.Date(NA)), na.rm = TRUE)
    ) |>
    select(-starts_with("l_"))
}

# --- score de classement -----------------------------------------------
# Visiteurs uniques 90 j, avec léger lissage bayésien pour ne pas classer
# une page à 2 visites sur du bruit : score = (v + C*m) / (1 + C) où m est
# la médiane des visiteurs des pages effectivement vues, C = 3. Un petit
# terme log(all-time) départage les ex aequo et récompense la profondeur
# d'historique.
shrink <- function(v) {
  vv <- v[v > 0]; m <- if (length(vv)) stats::median(vv) else 0; C <- 3
  (v + C * m) / (1 + C)
}
pageviews <- pageviews |>
  mutate(rank_score = shrink(visitors_90d) + 0.15 * log1p(pageviews_all)) |>
  arrange(desc(rank_score))

pageviews_source <- pageviews |>
  group_by(source) |>
  summarise(visitors_90d = sum(visitors_90d), pageviews_90d = sum(pageviews_90d),
            visitors_365d = sum(visitors_365d), pageviews_365d = sum(pageviews_365d),
            pageviews_all = sum(pageviews_all), .groups = "drop") |>
  mutate(rank_score = shrink(visitors_90d) + 0.15 * log1p(pageviews_all)) |>
  arrange(desc(rank_score))

message(sprintf("[_pageviews] %d (source,dataset), %d sources — top: %s",
                nrow(pageviews), nrow(pageviews_source),
                paste(head(pageviews_source$source, 5), collapse = ", ")))

save(pageviews, pageviews_source, file = out_file)
