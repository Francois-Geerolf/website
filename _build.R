#!/usr/bin/env Rscript
source(here::here("_rinit.R"))

# Fonction utilitaire pour formater un temps en minutes + secondes
formater_temps <- function(secondes) {
  minutes <- floor(secondes / 60)
  sec <- round(secondes %% 60)
  sprintf("%dm %02ds", minutes, sec)
}

# Trouver tous les fichiers .qmd dans le dossier "data" et ses sous-dossiers
# setwd("data")
fichiers <- list.files(
  path = ".",
  pattern = "\\.qmd$", 
  recursive = F, 
  full.names = TRUE
)

# Remove index.qmd
fichiers <- setdiff(fichiers, "./index.qmd")

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
