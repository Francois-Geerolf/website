#!/usr/bin/env Rscript

# Fonction utilitaire pour formater un temps en minutes + secondes
formater_temps <- function(secondes) {
  minutes <- floor(secondes / 60)
  sec <- round(secondes %% 60)
  sprintf("%dm %02ds", minutes, sec)
}

# Fonction pour compiler un fichier .qmd en mode silencieux, avec mesure du temps
compiler_qmd <- function(fichier) {
  if (!file.exists(fichier)) {
    return(list(file = fichier, success = FALSE, elapsed = NA_real_, error = "Fichier introuvable"))
  }
  start <- Sys.time()
  res <- tryCatch(
    {
      suppressWarnings(
        suppressMessages(
          capture.output(
            quarto::quarto_render(input = fichier, quiet = TRUE),
            file = NULL
          )
        )
      )
      list(success = TRUE, error = NA_character_)
    },
    error = function(e) list(success = FALSE, error = conditionMessage(e))
  )
  end <- Sys.time()
  elapsed <- if (isTRUE(res$success)) as.numeric(difftime(end, start, units = "secs")) else NA_real_
  list(file = fichier, success = res$success, elapsed = elapsed, error = res$error)
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
  for (x in resultats) {
    status_icon <- if (isTRUE(x$success)) "✅" else "❌"
    detail <- if (isTRUE(x$success)) formater_temps(x$elapsed) else paste0("Erreur: ", x$error)
    message(" ", status_icon, " ", x$file, " : ", detail)
  }
  message("\n🕒 Temps total : ", formater_temps(temps_total_sec))
}

# (Option CI) Faire échouer le job si un fichier échoue
# if (nb_ko > 0) quit(status = 1)



