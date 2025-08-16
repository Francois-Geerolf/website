#!/usr/bin/env Rscript

# Fonction utilitaire pour formater un temps en minutes + secondes
formater_temps <- function(secondes) {
  minutes <- floor(secondes / 60)
  sec <- round(secondes %% 60)
  sprintf("%dm %02ds", minutes, sec)
}

# Fonction pour compiler un fichier .qmd en mode silencieux, avec mesure du temps
# et capture des logs (stdout + messages/erreurs)
compiler_qmd <- function(fichier, log_tail_n = 25) {
  if (!file.exists(fichier)) {
    return(list(
      file = fichier, success = FALSE, elapsed = NA_real_,
      error = "Fichier introuvable", log_tail = character()
    ))
  }
  
  # Fichier temporaire pour capturer tout le log
  log_path <- tempfile(pattern = "quarto_render_", fileext = ".log")
  con <- file(log_path, open = "wt")
  on.exit({
    # Sécurité : rétablir les sinks si actifs
    for (t in c("message", "output")) {
      try(sink(type = t), silent = TRUE)
    }
    try(close(con), silent = TRUE)
  }, add = TRUE)
  
  # Redirige la sortie standard ET les messages/erreurs vers le log
  sink(con, type = "output")
  sink(con, type = "message")
  
  start <- Sys.time()
  res <- tryCatch(
    {
      # quiet = TRUE limite le bruit, mais on capture quand même tout au cas où
      quarto::quarto_render(input = fichier, quiet = TRUE)
      list(success = TRUE, error = NA_character_)
    },
    error = function(e) list(success = FALSE, error = conditionMessage(e))
  )
  end <- Sys.time()
  
  # Arrêt de la capture
  sink(type = "message")
  sink(type = "output")
  
  # Lecture du log complet et extraction de la fin (utile en cas d'erreur)
  log_lines <- tryCatch(readLines(log_path, warn = FALSE), error = function(e) character())
  log_tail <- if (length(log_lines)) tail(log_lines, log_tail_n) else character()
  
  elapsed <- if (isTRUE(res$success)) as.numeric(difftime(end, start, units = "secs")) else NA_real_
  
  list(
    file = fichier,
    success = res$success,
    elapsed = elapsed,
    error = res$error,
    log_tail = log_tail
  )
}

# Trouver tous les fichiers .qmd dans le dossier "data" et ses sous-dossiers
fichiers <- list.files(
  path = "data",
  pattern = "\\.qmd$",
  recursive = TRUE,
  full.names = TRUE
)

# Mesure du temps total
debut_total <- Sys.time()
resultats <- lapply(fichiers, compiler_qmd)
fin_total <- Sys.time()
temps_total_sec <- as.numeric(difftime(fin_total, debut_total, units = "secs"))

# Résumé final
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
  
  # Détails synthétiques (1 ligne par fichier)
  for (x in resultats) {
    status_icon <- if (isTRUE(x$success)) "✅" else "❌"
    detail <- if (isTRUE(x$success)) formater_temps(x$elapsed) else paste0("Erreur: ", x$error)
    message(" ", status_icon, " ", x$file, " : ", detail)
  }
  
  # En cas d'échec, afficher un extrait du log d'erreur (25 dernières lignes) pour chaque fichier KO
  if (nb_ko > 0) {
    message("\n🧾 Extrait des logs d'erreur (dernières lignes) :")
    for (x in resultats) {
      if (!isTRUE(x$success)) {
        message("\n——— ", x$file, " — LOG TAIL ———")
        if (length(x$log_tail)) {
          # Imprime ligne par ligne (évite le collapse qui peut tronquer)
          for (ln in x$log_tail) message(ln)
        } else {
          message("(aucun log capturé)")
        }
      }
    }
  }
  
  message("\n🕒 Temps total : ", formater_temps(temps_total_sec))
}

# (Option CI) Faire échouer le job si un fichier échoue
# if (nb_ko > 0) quit(status = 1)




