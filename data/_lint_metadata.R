#!/usr/bin/env Rscript
# Lint des data/<source>/_metadata.yml : signale les incohérences de config
# de section qui, sinon, passent inaperçues jusqu'à ce qu'une page rendue
# les trahisse (filtre d'écriture inclusive oublié, statcounter manquant...).
#
#   Rscript data/_lint_metadata.R            # rapport, code 1 si erreur "dure"
#   Rscript data/_lint_metadata.R --quiet    # n'affiche que les problèmes
#
# Appelé aussi (via try()) en fin de data/_datasets.R, donc à chaque refresh
# du catalogue. Ne fait jamais échouer un build -- juste du bruit visible.
#
# Il n'y a PAS de _quarto.yml : Quarto ne lit que le _metadata.yml du dossier
# de chaque .qmd, donc chaque dossier doit porter sa config en entier. Ce
# lint remplace la centralisation qu'offrirait un projet Quarto.

suppressWarnings(suppressMessages({
  library(yaml)
}))

root  <- path.expand("~/iCloud/website/data")
args  <- commandArgs(trailingOnly = TRUE)
quiet <- "--quiet" %in% args

`%||%` <- function(a, b) if (is.null(a)) b else a

files <- Sys.glob(file.path(root, "*", "_metadata.yml"))
if (!length(files)) { message("Aucun _metadata.yml trouvé sous ", root); quit(status = 0) }

# --- Règles -------------------------------------------------------------
# Chaque règle : name, severity ("error"|"warn"), test(meta, raw) -> TRUE si OK.
# `meta` = liste yaml parsée ; `raw` = texte brut (pour les tests sur chaîne).
has_str <- function(raw, s) grepl(s, raw, fixed = TRUE)

rules <- list(
  list(
    name = "genderize-fr (écriture inclusive)",
    severity = "error",
    # Concerné dès qu'il y a lang: fr -> le filtre doit être listé.
    applies = function(meta, raw) identical(tolower(as.character(meta$lang %||% "")), "fr"),
    test    = function(meta, raw) has_str(raw, "genderize-fr.lua")
  ),
  list(
    name = "statcounter (analytics)",
    severity = "warn",
    applies = function(meta, raw) TRUE,
    test    = function(meta, raw) has_str(raw, "statcounter.html")
  ),
  list(
    name = "thème _fgeerolf.scss",
    severity = "warn",
    applies = function(meta, raw) TRUE,
    test    = function(meta, raw) has_str(raw, "_fgeerolf.scss")
  ),
  list(
    name = "author: François Geerolf",
    severity = "warn",
    applies = function(meta, raw) TRUE,
    test    = function(meta, raw) has_str(raw, "François Geerolf")
  )
)

n_err <- 0L; n_warn <- 0L; n_ok <- 0L
report <- character()

for (f in sort(files)) {
  src  <- basename(dirname(f))
  raw  <- paste(readLines(f, warn = FALSE), collapse = "\n")
  meta <- tryCatch(yaml::yaml.load(raw), error = function(e) NULL)
  if (is.null(meta)) {
    n_err <- n_err + 1L
    report <- c(report, sprintf("✖ %-14s _metadata.yml : YAML illisible", src))
    next
  }
  probs <- character()
  for (r in rules) {
    if (!isTRUE(r$applies(meta, raw))) next
    if (isTRUE(r$test(meta, raw))) next
    if (r$severity == "error") { n_err <- n_err + 1L } else { n_warn <- n_warn + 1L }
    probs <- c(probs, sprintf("   %s %s",
                              if (r$severity == "error") "✖" else "·", r$name))
  }
  if (length(probs)) {
    report <- c(report, sprintf("%s %s", if (any(grepl("✖", probs))) "✖" else "·", src), probs)
  } else {
    n_ok <- n_ok + 1L
  }
}

if (length(report) && !(quiet && n_err == 0L && n_warn == 0L)) {
  cat("── lint data/<source>/_metadata.yml ──\n")
  cat(report, sep = "\n"); cat("\n")
}
cat(sprintf("%d dossiers OK · %d avertissement(s) · %d erreur(s)\n",
            n_ok, n_warn, n_err))
if (n_err > 0L)
  cat("→ corriger : ajouter la ligne manquante au _metadata.yml du dossier ",
      "(ex. filters:\\n  - ../../code/genderize-fr.lua après lang: fr)\n", sep = "")

quit(status = if (n_err > 0L) 1L else 0L)
