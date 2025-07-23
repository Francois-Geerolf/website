#!/usr/bin/env Rscript

# Compiler le fichier calendrier.qmd vers calendrier.html
if (file.exists("data/insee/calendrier.qmd")) {
  message("📄 Compilation de: data/insee/calendrier.qmd")
  quarto::quarto_render(input = "data/insee/calendrier.qmd")
} else {
  message("❌ Fichier data/insee/calendrier.qmd introuvable")
}

