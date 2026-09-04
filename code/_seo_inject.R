#!/usr/bin/env Rscript
# ---------------------------------------------------------------------------
# code/_seo_inject.R  —  post-render SEO pass over data/<source>/<ds>.html
# ---------------------------------------------------------------------------
# Adds, just before </head> of every catalogued dataset page:
#   * <meta name="description">      (regular SEO snippet)
#   * <link rel="canonical">
#   * <link rel="icon"> / apple-touch-icon  (so Google's result favicon is
#     the sharp UCLA seal, not whatever it guesses from /favicon.ico)
#   * <script type="application/ld+json"> schema.org/Dataset  <-- the point:
#     makes the page eligible for Google Dataset Search.
#
# Idempotent: the block is fenced with <!-- seo:begin/end --> and replaced
# in place on re-run. Only rewrites files whose block actually changed.
#
#   Rscript code/_seo_inject.R [HTML_ROOT]      (default: <repo>/data)
#
# Wired into data/_datasets.R (local, points at ~/iCloud/website/data) and
# the themes.yml / oecd.yml workflows (point at the freshly built data/).
# Needs only data/_datasets.RData, which is committed.
# ---------------------------------------------------------------------------

suppressWarnings(suppressMessages(library(dplyr)))
invisible(suppressWarnings(Sys.setlocale("LC_TIME", "C")))   # English month names

args      <- commandArgs(trailingOnly = TRUE)
html_root <- if (length(args) >= 1 && nzchar(args[1])) args[1] else here::here("data")
BASE      <- "https://fgeerolf.com"
dsf       <- here::here("data", "_datasets.RData")
if (!exists("datasets")) load(dsf)

# source slug -> (organisation name, single-country spatialCoverage or NA)
SRC <- list(
  acoss=c("Acoss (Urssaf Caisse nationale)","France"),
  acpr=c("Autorité de Contrôle Prudentiel et de Résolution","France"),
  ademe=c("ADEME","France"), aft=c("Agence France Trésor","France"),
  ameco=c("European Commission (AMECO)","European Union"),
  bdf=c("Banque de France","France"), bea=c("U.S. Bureau of Economic Analysis","United States"),
  bis=c("Bank for International Settlements",NA), bls=c("U.S. Bureau of Labor Statistics","United States"),
  boe=c("Bank of England","United Kingdom"), buba=c("Deutsche Bundesbank","Germany"),
  cbp=c("U.S. Census Bureau (County Business Patterns)","United States"),
  census=c("U.S. Census Bureau","United States"), cepii=c("CEPII",NA),
  citepa=c("Citepa","France"), comtrade=c("UN Comtrade",NA), cre=c("Commission de régulation de l'énergie","France"),
  crosswalks=c("Crosswalks",NA), dallas_fed=c("Federal Reserve Bank of Dallas","United States"),
  dares=c("Dares (Ministère du Travail)","France"), destatis=c("Statistisches Bundesamt (Destatis)","Germany"),
  dgafp=c("DGAFP (Ministère de la Fonction publique)","France"), douanes=c("Douanes françaises","France"),
  drees=c("DREES (Ministère de la Santé)","France"), dvf=c("Demandes de valeurs foncières (DGFiP)","France"),
  ec=c("European Commission","European Union"), ecb=c("European Central Bank","Euro area"),
  eurostat=c("Eurostat","European Union"), "fama-french"=c("Kenneth R. French Data Library","United States"),
  fhfa=c("Federal Housing Finance Agency","United States"), frb=c("Federal Reserve Board","United States"),
  "frb-ny"=c("Federal Reserve Bank of New York","United States"),
  fred=c("Federal Reserve Bank of St. Louis (FRED)",NA), freddie=c("Freddie Mac","United States"),
  gfd=c("Global Financial Data",NA), ilo=c("International Labour Organization",NA),
  imf=c("International Monetary Fund",NA), ined=c("Institut national d'études démographiques","France"),
  insee=c("Insee","France"), investing=c("Investing.com",NA),
  ipp=c("Institut des politiques publiques","France"), maddison=c("Maddison Project Database",NA),
  mtes=c("Ministère de la Transition écologique","France"), notaires=c("Notaires de France","France"),
  oecd=c("OECD",NA), olap=c("Observatoire des loyers de l'agglomération parisienne","France"),
  ons=c("Office for National Statistics","United Kingdom"), pwt=c("Penn World Table",NA),
  quandl=c("Nasdaq Data Link (Quandl)",NA), rba=c("Reserve Bank of Australia","Australia"),
  rei=c("Direction générale des collectivités locales (REI)","France"),
  sdes=c("SDES (Ministère de la Transition écologique)","France"),
  shiller=c("Robert J. Shiller (Yale)","United States"), statjp=c("Statistics Bureau of Japan","Japan"),
  un=c("United Nations",NA), undata=c("United Nations",NA), us=c("United States (various)","United States"),
  wb=c("World Bank",NA), wdi=c("World Bank (World Development Indicators)",NA),
  wid=c("World Inequality Database",NA), wto=c("World Trade Organization",NA),
  yahoo=c("Yahoo! Finance",NA), zillow=c("Zillow Research","United States")
)

# source slug -> license URL. Only the well-established open licences; a
# source that isn't here just omits `license` (Rich Results Test flags that
# as a non-critical "optional field missing", which is fine).
LIC <- local({
  etalab  <- "https://www.etalab.gouv.fr/licence-ouverte-open-licence/"
  usgov   <- "https://www.usa.gov/government-works"
  ogl     <- "http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/"
  ccby4   <- "https://creativecommons.org/licenses/by/4.0/"
  dlde    <- "https://www.govdata.de/dl-de/by-2-0"
  g <- list(
    c(c("acoss","acpr","ademe","aft","bdf","citepa","cre","dares","dgafp",
        "douanes","drees","dvf","ined","insee","ipp","mtes","notaires","olap",
        "rei","sdes"), etalab),
    c(c("bea","bls","cbp","census","dallas-fed","fhfa","frb","frb-ny","fred",
        "freddie","us","john-fernald-tfp"), usgov),
    c(c("ons","boe"), ogl),
    c(c("ameco","ec","eurostat","oecd","wb","wdi","wid","maddison","pwt"), ccby4),
    c(c("buba","destatis"), dlde))
  m <- character(0)
  for (grp in g) { u <- grp[length(grp)]; for (s in grp[-length(grp)]) m[s] <- u }
  m
})

j <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, pretty = FALSE, null = "null")

clean_title <- function(title, slug) {
  t <- trimws(as.character(title))
  if (is.na(t) || t == "" || t == "NA") return(slug)
  for (dash in c(" - ", " – ", " — ")) {
    suff <- paste0(dash, slug)
    if (endsWith(t, suff)) return(trimws(substr(t, 1, nchar(t) - nchar(suff))))
  }
  t
}
temporal <- function(title) {
  m <- regmatches(title, regexec("\\((\\d{4})\\s*[-–]\\s*(\\d{4})?\\)", title))[[1]]
  if (length(m) == 3) paste0(m[2], "/", if (nzchar(m[3])) m[3] else "..") else NULL
}

build_ld <- function(src, ds, row) {
  title <- clean_title(row$Title, ds)
  url   <- sprintf("%s/data/%s/%s.html", BASE, src, ds)
  org   <- SRC[[src]]; provider <- if (!is.null(org)) org[1] else toupper(src)
  space <- if (!is.null(org) && !is.na(org[2])) org[2] else NULL
  upd   <- suppressWarnings(as.Date(max(row$data_updated, as.Date(row$`.html`), na.rm = TRUE)))
  nobs  <- suppressWarnings(as.integer(row$Nobs))

  desc <- paste0(
    title, " — data from ", provider, ".",
    if (!is.na(nobs) && nobs > 0) paste0(" ", format(nobs, big.mark = ","), " observations.") else "",
    if (!is.na(upd)) paste0(" Updated ", format(upd, "%B %Y"), ".") else "",
    " Interactive chart and reproducible R code on fgeerolf.com."
  )

  ld <- list(
    "@context" = "https://schema.org", "@type" = "Dataset",
    name = title, description = desc, url = url,
    identifier = paste0(src, "/", ds),
    keywords = c(src, provider, "macroeconomics", "economic data", "time series"),
    isAccessibleForFree = TRUE,
    creator = list("@type" = "Person", name = "François Geerolf",
                   url = BASE, affiliation = list("@type" = "Organization",
                   name = "OFCE, Sciences Po")),
    publisher = list("@type" = "Organization", name = provider),
    includedInDataCatalog = list("@type" = "DataCatalog",
      name = "fgeerolf.com — macroeconomic data", url = paste0(BASE, "/data/")),
    isPartOf = sprintf("%s/data/%s/", BASE, src)
  )
  if (!is.na(upd)) ld$dateModified <- format(upd, "%Y-%m-%d")
  tc <- temporal(as.character(row$Title)); if (!is.null(tc)) ld$temporalCoverage <- tc
  if (!is.null(space)) ld$spatialCoverage <- list("@type" = "Place", name = space)
  if (!is.na(LIC[src])) ld$license <- unname(LIC[src])
  list(ld = ld, desc = desc, url = url)
}

BEG <- "<!-- seo:begin -->"; END <- "<!-- seo:end -->"

inject_one <- function(path, src, ds, row) {
  orig <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  b    <- build_ld(src, ds, row)

  # strip any previous block first, so re-runs are clean and the
  # "already has a description?" test looks only at Quarto's own output
  base <- orig
  i0 <- regexpr(BEG, base, fixed = TRUE)
  if (i0 > 0) {
    i1 <- regexpr(END, base, fixed = TRUE)
    if (i1 > 0) {
      tail <- sub("^\r?\n", "", substr(base, i1 + nchar(END), nchar(base)))
      base <- paste0(substr(base, 1, i0 - 1), tail)
    }
  }

  has_desc <- grepl('name=["\']description["\']', base)
  has_icon <- grepl('rel=["\'][^"\']*icon', base)
  block <- paste0(
    BEG, "\n",
    if (!has_desc) paste0('<meta name="description" content="',
                          gsub('"', "&quot;", b$desc, fixed = TRUE), '">\n') else "",
    '<link rel="canonical" href="', b$url, '">\n',
    if (!has_icon) paste0(
      '<link rel="icon" href="', BASE, '/favicon.ico" sizes="any">\n',
      '<link rel="icon" type="image/png" href="', BASE, '/favicon.png" sizes="96x96">\n',
      '<link rel="apple-touch-icon" href="', BASE, '/apple-touch-icon.png">\n') else "",
    '<script type="application/ld+json">',
    gsub("</", "<\\/", j(b$ld), fixed = TRUE), "</script>\n",
    END, "\n")

  hpos <- regexpr("</head>", base, ignore.case = TRUE)
  if (hpos < 1) return(FALSE)
  new <- paste0(substr(base, 1, hpos - 1), block, substr(base, hpos, nchar(base)))
  if (identical(new, orig)) return(FALSE)
  writeLines(new, path, useBytes = TRUE)
  TRUE
}

# --- walk -------------------------------------------------------------
cat <- datasets %>% filter(!is.na(source), nzchar(source), source != ".",
                           !is.na(dataset), nzchar(dataset))
n_ok <- 0L; n_seen <- 0L
for (src in unique(cat$source)) {
  dir <- file.path(html_root, src)
  if (!dir.exists(dir)) next
  rows <- cat[cat$source == src, ]
  for (i in seq_len(nrow(rows))) {
    ds <- rows$dataset[i]
    path <- file.path(dir, paste0(ds, ".html"))
    if (!file.exists(path)) next
    n_seen <- n_seen + 1L
    ok <- tryCatch(inject_one(path, src, ds, rows[i, ]),
                   error = function(e) { message("  ! ", src, "/", ds, ": ", e$message); FALSE })
    if (isTRUE(ok)) n_ok <- n_ok + 1L
  }
}
message(sprintf("[_seo_inject] %s : %d dataset pages seen, %d written",
                normalizePath(html_root, mustWork = FALSE), n_seen, n_ok))
