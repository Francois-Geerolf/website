#!/usr/bin/env Rscript

# Fonction pour compiler un fichier .qmd s’il existe
compiler_qmd <- function(fichier) {
  if (file.exists(fichier)) {
    message("📄 Compilation de ", fichier)
    quarto::quarto_render(input = fichier)
  } else {
    message("❌ Fichier ", fichier, " introuvable")
  }
}

# Liste des fichiers à compiler
fichiers <- c(
  "data/insee/calendrier.qmd",
  "data/taux-dinteret.qmd"
)

# Compilation
lapply(fichiers, compiler_qmd)
