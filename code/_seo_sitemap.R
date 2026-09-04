#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# code/_seo_sitemap.R  ->  sitemap.xml + robots.txt   (repo root)
# ---------------------------------------------------------------------------
# A full sitemap so Google (and Google Dataset Search, via the per-page
# schema.org/Dataset markup that code/_seo_inject.R adds) can reach every
# /data/<source>/<dataset>.html and /data/<theme>.html — the old hand-kept
# sitemap listed ~13 URLs.
#
#   /data/**            from the catalog (_datasets.RData + _themes.RData)
#   everything else     every tracked .qmd -> its .html URL, lastmod from git
#
# Run standalone     :  Rscript code/_seo_sitemap.R
# or let data/_datasets.R / _build.R source() it.
# ---------------------------------------------------------------------------

suppressWarnings(suppressMessages(library(dplyr)))

root  <- here::here()
source(file.path(root, "code", "_seo_common.R"))
BASE  <- SEO_BASE
as_day <- seo_as_day
dsf   <- file.path(root, "data", "_datasets.RData")
thf   <- file.path(root, "data", "_themes.RData")
if (!exists("datasets") && file.exists(dsf)) load(dsf)
if (!exists("themes")   && file.exists(thf)) load(thf)

today <- format(Sys.Date())

urls <- list()   # list of c(loc, lastmod)
add  <- function(loc, lastmod = NA) urls[[length(urls) + 1L]] <<- c(loc, lastmod)

# --- /data/ from the catalog --------------------------------------------
add(paste0(BASE, "/data/"), today)

if (exists("datasets") && nrow(datasets)) {
  d <- datasets %>%
    filter(!is.na(source), nzchar(source), source != ".",
           !is.na(dataset), nzchar(dataset)) %>%
    mutate(lm = pmax(as.Date(data_updated), as.Date(`.html`), na.rm = TRUE))
  for (i in seq_len(nrow(d)))
    add(sprintf("%s/data/%s/%s.html", BASE, d$source[i], d$dataset[i]), as_day(d$lm[i]))
  s <- d %>% group_by(source) %>% summarise(lm = max(lm, na.rm = TRUE), .groups = "drop")
  for (i in seq_len(nrow(s)))
    add(sprintf("%s/data/%s/", BASE, s$source[i]), as_day(s$lm[i]))
}

if (exists("themes") && nrow(themes)) {
  for (i in seq_len(nrow(themes))) {
    q <- file.path(root, "data", paste0(themes$theme[i], ".qmd"))
    add(sprintf("%s/data/%s.html", BASE, themes$theme[i]),
        as_day(if (file.exists(q)) file.info(q)$mtime else NA))
  }
}

# Curated sources the auto-catalog misses (wid, crsp, maddison, uk, ...).
# Only list one whose landing page actually renders — a built index.html in
# the local render tree, or a source index.qmd in the repo (never a bare
# dir: e.g. data/wrds/ exists but serves 403).
local({
  cur <- seo_curated_sources(file.path(root, "data"))$id
  have_ds <- if (exists("datasets")) unique(datasets$source) else character()
  render_root <- path.expand("~/iCloud/website/data")
  for (id in setdiff(cur, have_ds)) {
    ok <- file.exists(file.path(root, "data", id, "index.qmd")) ||
          file.exists(file.path(render_root, id, "index.html"))
    if (ok) add(sprintf("%s/data/%s/", BASE, id), today)
  }
})

# --- everything else: tracked .qmd outside data/<source|theme> ----------
git <- function(...) tryCatch(
  system2("git", c("-C", shQuote(root), ...), stdout = TRUE, stderr = FALSE),
  error = function(e) character())

qmds <- git("ls-files", shQuote("*.qmd"))

# one git call: most-recent commit date per path
gitdates <- local({
  raw <- git("log", "--no-merges", shQuote("--format=%H|%cs"),
             "--name-only", "--", shQuote("*.qmd"))
  d <- character(); cur <- NA
  for (ln in raw) {
    if (grepl("^[0-9a-f]{7,40}\\|", ln)) cur <- sub("^[^|]+\\|", "", ln)
    else if (nzchar(ln) && is.na(d[ln])) d[ln] <- cur
  }
  d
})

# A .qmd is a standalone deployed page only if it opens with a YAML front
# matter fence. The many files under publications/, working-papers/,
# teaching/, press/, presentations/, public-debate/ are bodyless fragments
# `{{< include >}}`-d into a single listing page — they 404 on their own.
has_frontmatter <- function(p) {
  f <- file.path(root, p)
  if (!file.exists(f)) return(FALSE)
  ln <- readLines(f, n = 5, warn = FALSE, encoding = "UTF-8")
  ln <- ln[nzchar(trimws(ln))]
  length(ln) > 0 && grepl("^﻿?---\\s*$", ln[1])
}
skip_qmd <- function(p) {
  grepl("(^|/)_[^/]*$", p)          ||   # _foo.qmd helper/include
  grepl("(^|/)_[^/]*/", p)          ||   # something inside a _dir/
  grepl("_files/", p, fixed = TRUE) ||
  grepl("^data/", p)               ||   # /data/** handled by the catalog above
  grepl("^(code|files|flags|icon|cnis)/", p) ||
  !has_frontmatter(p)
}
qmd_url <- function(p) {
  u <- sub("\\.qmd$", ".html", p)
  u <- sub("(^|/)index\\.html$", "\\1", u)   # index.qmd -> dir root
  paste0(BASE, "/", u)
}

for (p in qmds) {
  if (skip_qmd(p)) next
  add(qmd_url(p), if (!is.na(gitdates[p])) gitdates[p] else today)
}

# --- de-dupe, write -----------------------------------------------------
m <- do.call(rbind, urls)
m <- m[!duplicated(m[, 1]), , drop = FALSE]
m <- m[order(m[, 1]), , drop = FALSE]

esc <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE);  gsub(">", "&gt;", s, fixed = TRUE)
}
body <- apply(m, 1, function(r) {
  paste0("  <url>\n    <loc>", esc(r[1]), "</loc>\n",
         if (!is.na(r[2])) paste0("    <lastmod>", r[2], "</lastmod>\n") else "",
         "  </url>")
})
writeLines(c('<?xml version="1.0" encoding="UTF-8"?>',
             '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">',
             body, "</urlset>"),
           file.path(root, "sitemap.xml"), useBytes = TRUE)

writeLines(c("User-agent: *", "Allow: /", "",
             paste0("Sitemap: ", BASE, "/sitemap.xml")),
           file.path(root, "robots.txt"), useBytes = TRUE)

message(sprintf("[_seo_sitemap] %d URLs -> sitemap.xml (+ robots.txt)", nrow(m)))
