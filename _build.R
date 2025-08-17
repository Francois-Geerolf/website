#!/usr/bin/env Rscript

# -------- compilation par fichier (capture logs; retry sans --quiet) --------
compiler_qmd <- function(fichier, log_tail_n = 50) {
  if (!file.exists(fichier)) {
    return(list(
      file = fichier, success = FALSE, elapsed = NA_real_,
      error = "Fichier introuvable", log_tail = character(), log_path = NA_character_
    ))
  }
  
  # Trouver le binaire Quarto
  find_quarto <- function() {
    p <- tryCatch(quarto::quarto_path(), error = function(e) "")
    if (nzchar(p)) return(p)
    p <- Sys.which("quarto")
    if (nzchar(p)) return(p)
    stop("Quarto CLI introuvable. Installez Quarto et/ou ajoutez-le au PATH.")
  }
  cmd <- find_quarto()
  
  # On travaille dans le dossier du .qmd (chemins relatifs)
  owd <- getwd()
  on.exit(setwd(owd), add = TRUE)
  setwd(dirname(fichier))
  input_base <- basename(fichier)
  
  run_once <- function(quiet_flag = TRUE) {
    # log temporaire unique par tentative
    log_path <- tempfile(pattern = paste0(gsub("[^A-Za-z0-9_-]", "_", input_base), "_"), fileext = ".log")
    args <- c("render", input_base)
    if (quiet_flag) args <- c(args, "--quiet")
    
    t0 <- Sys.time()
    if (requireNamespace("processx", quietly = TRUE)) {
      res <- tryCatch(
        processx::run(
          cmd, args,
          stderr_to_stdout = TRUE, echo = FALSE,
          error_on_status = FALSE, windows_verbatim_args = TRUE,
          env = c("QUARTO_DONT_PRETTY" = "1") # force sortie "brute"
        ),
        error = function(e) NULL
      )
      if (!is.null(res)) {
        writeLines(res$stdout %||% "", log_path)
        status <- as.integer(res$status %||% 999L)
      } else {
        status <- tryCatch(system2(cmd, args, stdout = log_path, stderr = log_path, wait = TRUE),
                           error = function(e) 999L)
      }
    } else {
      status <- tryCatch(system2(cmd, args, stdout = log_path, stderr = log_path, wait = TRUE),
                         error = function(e) 999L)
    }
    t1 <- Sys.time()
    
    list(status = status,
         elapsed = as.numeric(difftime(t1, t0, units = "secs")),
         log_path = log_path,
         log_lines = if (file.exists(log_path)) readLines(log_path, warn = FALSE) else character())
  }
  
  # 1) tentative silencieuse
  a <- run_once(quiet_flag = TRUE)
  
  # 2) si échec ET log vide, on relance sans --quiet pour obtenir le détail
  need_retry <- (a$status != 0L) && (length(a$log_lines) == 0L)
  b <- if (need_retry) run_once(quiet_flag = FALSE) else NULL
  res <- if (is.null(b)) a else b
  
  success <- is.numeric(res$status) && res$status == 0L
  
  # extraction d'un message d'erreur synthétique
  err_lines <- grep(
    "(^!|^x\\s|^Error\\b|^Erreur\\b|^error\\b|Execution halted|pandoc error|LaTeX Error)",
    res$log_lines, value = TRUE, ignore.case = TRUE
  )
  brief <- if (length(err_lines)) trimws(tail(err_lines, 1)) else NA_character_
  
  list(
    file = fichier,
    success = success,
    elapsed = if (success) res$elapsed else NA_real_,
    error = brief,
    log_tail = tail(res$log_lines, log_tail_n),
    log_path = res$log_path
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


