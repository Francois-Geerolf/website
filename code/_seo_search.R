#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# code/_seo_search.R  ->  data/search.json
# ---------------------------------------------------------------------------
# One flat JSON index of the whole catalog (every source, dataset and theme)
# for the client-side search box in data/index.qmd (data/search.js). Static
# file, no server: regenerated whenever the catalog changes.
#
# Run standalone            :  Rscript code/_seo_search.R
# or let data/_datasets.R source() it after it rewrites _datasets.RData.
#
# Record shape (keys kept short, the file ships to every visitor):
#   k  kind        "s" source | "d" dataset | "t" theme
#   s  source/theme slug
#   d  dataset slug           (kind "d" only)
#   t  title / label
#   u  absolute URL
#   m  last-modified date "YYYY-MM-DD"  (omitted when unknown)
#   r  rank_score (audience) rounded 3dp (omitted when unknown / 0)
# ---------------------------------------------------------------------------

suppressWarnings(suppressMessages(library(dplyr)))

root      <- here::here()
data_dir  <- file.path(root, "data")
out_file  <- file.path(data_dir, "search.json")
source(file.path(root, "code", "_seo_common.R"))
BASE      <- SEO_BASE
as_day    <- seo_as_day
clean_title <- function(title, slug) seo_clean_title(title, slug)

if (!exists("datasets")) load(file.path(data_dir, "_datasets.RData"))
if (!exists("themes"))   load(file.path(data_dir, "_themes.RData"))

# Country / language hints so "german inflation", "french gdp", "uk debt"…
# match even though the titles say "Germany" / nothing / "United Kingdom".
SRC_KW <- c(
  acoss = "france french", acpr = "france french", ademe = "france french",
  aft = "france french", bdf = "france french banque de france",
  insee = "france french", ipp = "france french", ined = "france french",
  dares = "france french", drees = "france french", dvf = "france french",
  cre = "france french", mtes = "france french", sdes = "france french",
  olap = "france french paris", rei = "france french", citepa = "france french",
  douanes = "france french", dgafp = "france french", notaires = "france french",
  bea = "united states us usa american", bls = "united states us usa american",
  frb = "united states us usa american federal reserve",
  "frb-ny" = "united states us usa american",
  fred = "united states us usa american", census = "united states us usa american",
  cbp = "united states us usa american", fhfa = "united states us usa american",
  freddie = "united states us usa american", zillow = "united states us usa american",
  shiller = "united states us usa american", "fama-french" = "united states us usa american",
  us = "united states us usa american", "dallas-fed" = "united states us usa american",
  "john-fernald-tfp" = "united states us usa american",
  buba = "germany german deutschland", destatis = "germany german deutschland",
  ons = "united kingdom uk britain british", boe = "united kingdom uk britain british",
  ecb = "euro area eurozone europe european", eurostat = "european union europe eu european",
  ec = "european union europe eu european", ameco = "european union europe eu european",
  statjp = "japan japanese", rba = "australia australian")

# --- datasets --------------------------------------------------------------
ds <- datasets %>%
  filter(!is.na(source), nzchar(source), source != ".",
         !is.na(dataset), nzchar(dataset)) %>%
  transmute(
    k = "d",
    s = source,
    d = dataset,
    t = clean_title(Title, dataset),
    c = unname(SRC_KW[source]),
    u = sprintf("%s/data/%s/%s.html", BASE, source, dataset),
    m = as_day(pmax(as.Date(data_updated), as.Date(`.html`), na.rm = TRUE)),
    r = ifelse(is.na(rank_score), NA_real_, round(rank_score, 3))
  )

curated <- seo_curated_sources(data_dir)

src_n <- datasets %>%
  filter(!is.na(source), nzchar(source), source != ".") %>%
  group_by(s = source) %>%
  summarise(n = dplyr::n(),
            m = as_day(max(pmax(as.Date(data_updated), as.Date(`.html`),
                                na.rm = TRUE), na.rm = TRUE)),
            .groups = "drop")

all_src <- union(src_n$s, curated$id)
lbl <- setNames(curated$label, curated$id)
src <- tibble(s = all_src) %>%
  left_join(src_n, by = "s") %>%
  transmute(
    k = "s", s, d = NA_character_,
    t = {
      base <- ifelse(s %in% names(lbl), unname(lbl[s]), s)
      ifelse(!is.na(n), sprintf("%s — %d dataset%s", base, n, ifelse(n == 1, "", "s")), base)
    },
    c = unname(SRC_KW[s]),
    u = sprintf("%s/data/%s/", BASE, s),
    m, r = NA_real_
  )

# --- themes ---------------------------------------------------------------
th <- themes %>%
  transmute(
    k = "t", s = theme, d = NA_character_,
    t = clean_title(Title, theme),
    u = sprintf("%s/data/%s.html", BASE, theme),
    m = as_day(suppressWarnings(
          as.Date(file.info(file.path(data_dir, paste0(theme, ".qmd")))$mtime))),
    r = NA_real_
  )

idx <- bind_rows(src, th, ds)

# Drop all-NA optional keys row-by-row so the payload stays small.
records <- lapply(seq_len(nrow(idx)), function(i) {
  row <- as.list(idx[i, ])
  row <- row[!vapply(row, function(v) is.na(v) || (is.character(v) && !nzchar(v)),
                     logical(1))]
  row
})

# Source slugs that have a logo at /data/logos/<slug>.png — the widget shows
# it next to the result so you can tell insee / eurostat / oecd apart at a
# glance. Shipped once as a top-level list, not per record.
logos <- sub("\\.png$", "", list.files(file.path(data_dir, "logos"),
                                       pattern = "\\.png$"))
logos <- sort(logos[logos %in% unique(idx$s)])

json  <- jsonlite::toJSON(records, auto_unbox = TRUE, null = "null")
lgj   <- jsonlite::toJSON(logos, auto_unbox = FALSE)
writeLines(paste0("{\"generated\":\"", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                  "\",\"n\":", length(records),
                  ",\"logos\":", lgj,
                  ",\"items\":", json, "}"),
           out_file, useBytes = TRUE)

message(sprintf("[_seo_search] %d items (%d sources, %d themes, %d datasets), %d logos -> %s",
                length(records), nrow(src), nrow(th), nrow(ds), length(logos),
                sub(paste0("^", root, "/?"), "", out_file)))
