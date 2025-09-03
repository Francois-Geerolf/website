#!/usr/bin/env Rscript
source(here::here("_rinit.R"))

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
      quarto::quarto_render(input = fichier, quiet = FALSE)
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
  path = ".",
  pattern = "\\.qmd$", 
  recursive = F, 
  full.names = T
)

# Remove index.qmd
fichiers <- setdiff(fichiers, "./index.qmd")
fichiers <- c(fichiers, "./data/insee/calendrier.qmd")

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
