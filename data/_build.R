#!/usr/bin/env Rscript
source(here::here("_rinit.R"))

quiet <- FALSE

# Fonction utilitaire pour formater un temps en minutes + secondes
formater_temps <- function(secondes) {
  minutes <- floor(secondes / 60)
  sec <- round(secondes %% 60)
  sprintf("%dm %02ds", minutes, sec)
}

# Aller à la racine du dépôt GitHub
# setwd(Sys.getenv("GITHUB_WORKSPACE"))

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
  recursive = T, 
  full.names = T
)

# Move "./themes.qmd" to last
fichiers <- c(setdiff(fichiers, "data/themes.qmd"), "data/themes.qmd")
fichiers <- c(setdiff(fichiers, "data/index.qmd"), "data/index.qmd")

# Remove oecd
fichiers <- fichiers[!str_detect(fichiers, "^data/oecd")]

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
