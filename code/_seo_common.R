#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# code/_seo_common.R — shared helpers for _seo_search.R / _seo_sitemap.R /
# _seo_inject.R.  source()d, defines functions only.
# ---------------------------------------------------------------------------

SEO_BASE <- "https://fgeerolf.com"

seo_as_day <- function(x) {
  d <- suppressWarnings(as.Date(x))
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

# "Real title - <slug>" -> "Real title" ; blank / "NA" -> slug
seo_clean_title <- function(title, slug) {
  t <- trimws(as.character(title))
  bad <- is.na(t) | t == "" | t == "NA"
  t[bad] <- slug[bad]
  for (dash in c(" - ", " – ", " — ")) {
    suff <- paste0(dash, slug)
    hit  <- endsWith(t, suff)
    t[hit] <- trimws(substr(t[hit], 1, nchar(t[hit]) - nchar(suff[hit])))
  }
  t
}

# The hand-kept tibble(id=, label=) blocks in data/index.qmd — sources the
# auto-catalog misses (wid, crsp, maddison, wrds, uk, ...) plus a label to
# search / describe them by. Returns data.frame(id, label); never errors.
seo_curated_sources <- function(data_dir) {
  f <- file.path(data_dir, "index.qmd")
  empty <- data.frame(id = character(), label = character(), stringsAsFactors = FALSE)
  if (!file.exists(f)) return(empty)
  qtxt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  slice_call <- function(name) {
    st <- regexpr(paste0(name, "[ \t]*<-[ \t]*tibble\\("), qtxt)
    if (st < 0) return(NULL)
    cs <- strsplit(substring(qtxt, st + attr(st, "match.length") - 1L), "")[[1]]
    depth <- 0L; instr <- FALSE; out <- character(0)
    for (ch in cs) {
      out <- c(out, ch)
      if (instr) { if (ch == '"') instr <- FALSE; next }
      if (ch == '"') { instr <- TRUE; next }
      if (ch == "(") depth <- depth + 1L
      else if (ch == ")") { depth <- depth - 1L; if (depth == 0L) break }
    }
    paste0(name, " <- tibble", paste(out, collapse = ""))
  }
  e <- new.env(); e$tibble <- function(...) data.frame(..., stringsAsFactors = FALSE)
  rows <- lapply(
    c("main_datasets", "api", "france", "databanks", "microeconomic_datasets"),
    function(nm) tryCatch(eval(parse(text = slice_call(nm)), e)[, c("id", "label")],
                          error = function(err) NULL))
  d <- unique(do.call(rbind, rows))
  if (is.null(d)) return(empty)
  d[!d$id %in% c("rdb"), ]     # "rdb" is a dbnomics alias, has no page
}
