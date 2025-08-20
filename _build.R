#!/usr/bin/env Rscript

# Update OECD Databases
base_path <- here("data", "oecd")
scripts   <- list.files(base_path, pattern = "\\.R$", full.names = TRUE)

# Global timer
global_start <- Sys.time()

for (script in scripts) {
  if (file.exists(script)) {
    start <- Sys.time()
    tryCatch({
      source(script, local = TRUE)
      elapsed <- difftime(Sys.time(), start, units = "secs")
      message("✅  Sourced: ", basename(script), 
              " (", round(elapsed, 2), "s)")
    }, error = function(e) {
      message("❌  Failed: ", basename(script), 
              " — ", e$message)
    })
  } else {
    message("❌  File not found: ", basename(script))
  }
}

# Global elapsed time
global_elapsed <- difftime(Sys.time(), global_start, units = "secs")
message("\n⏱️  Total time: ", round(global_elapsed, 2), "s")

# Aller à la racine du dépôt GitHub
# setwd(Sys.getenv("GITHUB_WORKSPACE"))

# Fonction utilitaire pour formater un temps en minutes + secondes
formater_temps <- function(secondes) {
  minutes <- floor(secondes / 60)
  sec <- round(secondes %% 60)
  sprintf("%dm %02ds", minutes, sec)
}

# Fonction pour compiler un fichier .qmd, s’il existe, avec mesure du temps
compiler_qmd <- function(fichier) {
  if (file.exists(fichier)) {
    message("📄 Compilation de ", fichier)
    start <- Sys.time()
    ok <- tryCatch({
      quarto::quarto_render(input = fichier, quiet = quiet)
      TRUE
    }, error = function(e) {
      message("❌ Erreur compilation : ", e$message)
      FALSE
    })
    end <- Sys.time()
    elapsed_sec <- as.numeric(difftime(end, start, units = "secs"))
    if (ok) {
      message("✅ Succès (", formater_temps(elapsed_sec), ")")
      return(list(success = TRUE, time = elapsed_sec))
    } else {
      return(list(success = FALSE, time = NA))
    }
  } else {
    message("❌ Fichier ", fichier, " introuvable")
    return(list(success = FALSE, time = NA))
  }
}

# Trouver tous les fichiers .qmd dans le dossier "data" et ses sous-dossiers
# setwd("data")
fichiers <- list.files(
  path = "data",
  pattern = "\\.qmd$", 
  recursive = TRUE, 
  full.names = TRUE
)

# Move "./themes.qmd" to last
fichiers <- c(setdiff(fichiers, "data/themes.qmd"), "data/themes.qmd")
fichiers <- c(setdiff(fichiers, "data/index.qmd"), "data/index.qmd")

# Mesure du temps total
debut_total <- Sys.time()
resultats <- lapply(fichiers, compiler_qmd)
fin_total <- Sys.time()

# Calcul du temps total
temps_total_sec <- as.numeric(difftime(fin_total, debut_total, units = "secs"))

# Résumé final
message("\n📋 Résumé des temps de compilation :")
for (i in seq_along(fichiers)) {
  nom <- fichiers[[i]]
  res <- resultats[[i]]
  if (res$success) {
    message(" - ✅ ", nom, " : ", formater_temps(res$time))
  } else {
    message(" - ❌ ", nom, " : Erreur / fichier manquant")
  }
}
message("\n🕒 Temps total de compilation : ", formater_temps(temps_total_sec))
