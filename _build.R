#!/usr/bin/env Rscript

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
    quarto::quarto_render(input = fichier)
    end <- Sys.time()
    elapsed_sec <- as.numeric(difftime(end, start, units = "secs"))
    message("⏱️ Temps écoulé pour ", fichier, " : ", formater_temps(elapsed_sec))
    return(elapsed_sec)
  } else {
    message("❌ Fichier ", fichier, " introuvable")
    return(NA)
  }
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
temps_par_fichier <- lapply(fichiers, compiler_qmd)
fin_total <- Sys.time()

# Calcul du temps total
temps_total_sec <- as.numeric(difftime(fin_total, debut_total, units = "secs"))

# Résumé final
message("\n📋 Résumé des temps de compilation :")
for (i in seq_along(fichiers)) {
  nom <- fichiers[[i]]
  temps <- temps_par_fichier[[i]]
  if (!is.na(temps)) {
    message(" - ", nom, " : ", formater_temps(temps))
  } else {
    message(" - ", nom, " : ❌ Erreur / fichier manquant")
  }
}
message("\n🕒 Temps total de compilation : ", formater_temps(temps_total_sec))

