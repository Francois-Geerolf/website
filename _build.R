#!/usr/bin/env Rscript

# Option : commenter si tu veux voir l'avalanche de warnings R en fin de script
# options(warn = -1)

# -------- utilitaires --------
formater_temps <- function(secondes) {
  minutes <- floor(secondes / 60)
  sec <- round(secondes %% 60)
  sprintf("%dm %02ds", minutes, sec)
}

find_quarto <- function() {
  # Essaie via le package, puis via PATH
  p <- tryCatch(quarto::quarto_path(), error = function(e) "")
  if (nzchar(p)) return(p)
  p <- Sys.which("quarto")
  if (nzchar(p)) return(p)
  stop("Quarto CLI introuvable. Installez Quarto et/ou ajoutez-le au PATH.")
}

# -------- compilation par fichier (via CLI) --------
compiler_qmd <- function(fichier, log_tail_n = 40, quiet = TRUE) {
  if (!file.exists(fichier)) {
    return(list(
      file = fichier, success = FALSE, elapsed = NA_real_,
      error = "Fichier introuvable", log_tail = character(), log_path = NA_character_
    ))
  }
  
  # log par fichier
  log_path <- tempfile(pattern = paste0(gsub("[^A-Za-z0-9_-]", "_", basename(fichier)), "_"), fileext = ".log")
  
  # se placer dans le dossier du fichier pour gérer les chemins relatifs
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(dirname(fichier))
  
  cmd <- find_quarto()
  args <- c("render", basename(fichier))
  if (quiet) args <- c(args, "--quiet")
  
  start <- Sys.time()
  status <- tryCatch(
    system2(cmd, args, stdout = log_path, stderr = log_path, wait = TRUE),
    error = function(e) 999L
  )
  end <- Sys.time()
  
  success <- is.numeric(status) && status == 0L
  log_lines <- if (file.exists(log_path)) readLines(log_path, warn = FALSE) else character()
  
  # extrait d'erreur synthétique (dernière ligne "parlante")
  err_lines <- grep(
    pattern = "(^!|^x\\s|^Error\\b|^Erreur\\b|^error\\b|Execution halted|pandoc error|LaTeX Error)",
    x = log_lines, value = TRUE, ignore.case = TRUE
  )
  brief <- if (length(err_lines)) trimws(tail(err_lines, 1)) else NA_character_
  
  elapsed <- if (success) as.numeric(difftime(end, start, units = "secs")) else NA_real_
  
  list(
    file = fichier,
    success = success,
    elapsed = elapsed,
    error = brief,
    log_tail = tail(log_lines, log_tail_n),
    log_path = log_path
  )
}

# -------- trouver les .qmd --------
fichiers <- list.files(
  path = "data",
  pattern = "\\.qmd$",
  recursive = TRUE,
  full.names = TRUE
)

# -------- exécution --------
debut_total <- Sys.time()
resultats <- lapply(fichiers, compiler_qmd)
fin_total <- Sys.time()
temps_total_sec <- as.numeric(difftime(fin_total, debut_total, units = "secs"))

# -------- résumé --------
nb_total <- length(resultats)
nb_ok <- sum(vapply(resultats, function(x) isTRUE(x$success), logical(1)))
nb_ko <- nb_total - nb_ok
tout_ok <- (nb_total > 0 && nb_ko == 0)
ticker_global <- if (tout_ok) "✅" else "❌"

message("\n📋 Résumé de compilation")
if (nb_total == 0) {
  message("Aucun fichier .qmd trouvé.")
} else {
  message(sprintf("%s %d succès, %d échec(s) sur %d fichiers.", ticker_global, nb_ok, nb_ko, nb_total))
  
  # 1 ligne par fichier
  for (x in resultats) {
    status_icon <- if (isTRUE(x$success)) "✅" else "❌"
    detail <- if (isTRUE(x$success)) {
      formater_temps(x$elapsed)
    } else {
      # montre une erreur brève si dispo, sinon un placeholder
      paste0("Erreur: ", if (is.na(x$error) || !nzchar(x$error)) "(voir log)" else x$error)
    }
    message(" ", status_icon, " ", x$file, " : ", detail)
  }
  
  # logs détaillés pour les fichiers en échec
  if (nb_ko > 0) {
    message("\n🧾 Extrait des logs d'erreur (dernières lignes) :")
    for (x in resultats) {
      if (!isTRUE(x$success)) {
        message("\n——— ", x$file, " — LOG TAIL ———")
        if (length(x$log_tail)) {
          for (ln in x$log_tail) message(ln)
          message("\n(chemin du log complet : ", x$log_path, ")")
        } else {
          message("(aucun log capturé)")
        }
      }
    }
  }
  
  message("\n🕒 Temps total : ", formater_temps(temps_total_sec))
}

# (CI) échouer si au moins un fichier échoue
# if (nb_ko > 0) quit(status = 1)


