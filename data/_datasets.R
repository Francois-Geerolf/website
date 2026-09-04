source(here::here("_rinit.R"))
setwd(here::here("data"))

# Set your root folder here
root_dir <- "~/iCloud/website/data"

# Required core (case-insensitive) + allowed data
targets_core <- c("qmd", "html")
targets_data <- c("rdata", "parquet")

# --- Helper: robustly extract title from a .qmd's YAML front matter ---
get_qmd_title <- function(qmd_path) {
  if (is.na(qmd_path) || !nzchar(qmd_path) || !file.exists(qmd_path)) return(NA_character_)
  lines <- tryCatch(readLines(qmd_path, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) character())
  if (length(lines) == 0) return(NA_character_)
  
  # Strip UTF-8 BOM if present on first line
  lines[1] <- sub("^\ufeff", "", lines[1], perl = TRUE)
  
  # Search for YAML front matter fence within first ~200 lines
  ncheck <- min(200L, length(lines))
  top_idx <- which(grepl("^---\\s*$", lines[seq_len(ncheck)]))
  if (length(top_idx) > 0) {
    start <- top_idx[1]
    after <- (start + 1):length(lines)
    if (length(after) > 0) {
      end_rel <- which(grepl("^---\\s*$|^\\.\\.\\.\\s*$", lines[after]))
      if (length(end_rel) > 0) {
        end <- (start + end_rel[1])
        ymllines <- lines[(start + 1):(end - 1)]
        y <- tryCatch(yaml::yaml.load(paste(ymllines, collapse = "\n")),
                      error = function(e) NULL)
        if (!is.null(y)) {
          # Try common locations for title
          candidates <- list(y$title, y$pagetitle,
                             tryCatch(y$meta$title, error = function(e) NULL),
                             tryCatch(y$metadata$title, error = function(e) NULL))
          candidates <- candidates[!vapply(candidates, is.null, logical(1))]
          if (length(candidates) > 0) {
            return(as.character(candidates[[1]])[1])
          }
        }
      }
    }
  }
  
  # Fallback: look for a 'title:' line near the top (ignoring fences)
  idx <- which(grepl("^\\s*title\\s*:\\s*.+\\s*$", lines[seq_len(ncheck)], ignore.case = TRUE))[1]
  if (!is.na(idx)) {
    raw <- sub("^\\s*title\\s*:\\s*", "", lines[idx], ignore.case = TRUE, perl = TRUE)
    raw <- trimws(raw)
    raw <- sub('^"(.*)"$', "\\1", raw)
    raw <- sub("^'(.*)'$", "\\1", raw)
    return(raw)
  }
  
  NA_character_
}

# List candidate files recursively, ignoring case on pattern
files <- list.files(
  root_dir,
  pattern = "\\.(qmd|html|rdata|parquet)$",
  ignore.case = TRUE,
  recursive = TRUE,
  full.names = TRUE
)

# Drop files that live directly in root_dir (not in a source subfolder),
# e.g. energy.qmd, commodities.qmd -- these are cross-source synthesis
# pages, not per-source datasets, so they don't belong in a catalog meant
# to tell each source folder's index.qmd its own file count / last update.
files <- files[dirname(normalizePath(files, winslash = "/", mustWork = FALSE)) !=
                 normalizePath(root_dir, winslash = "/", mustWork = FALSE)]

# Build a data frame of parts
df <- tibble(path = files) %>%
  mutate(
    rel_dir = {
      rd <- normalizePath(root_dir, winslash = "/", mustWork = FALSE)
      pdir <- normalizePath(dirname(path), winslash = "/", mustWork = FALSE)
      out  <- str_replace(pdir, paste0("^", rd), "")
      out  <- str_replace(out, "^/?", "")
      ifelse(out == "", ".", out)
    },
    fname   = basename(path),
    ext_raw = tools::file_ext(fname),
    ext     = tolower(ext_raw),
    stem    = tools::file_path_sans_ext(fname),
    basepath = file.path(rel_dir, stem)
  )

# Identify basenames that have BOTH core files and AT LEAST ONE data file
complete_bases <- df %>%
  group_by(basepath) %>%
  summarize(have_ext = list(sort(unique(ext))), .groups = "drop") %>%
  filter(map_lgl(have_ext, ~ all(targets_core %in% .x) && any(.x %in% targets_data))) %>%
  pull(basepath)

# --- Per-file Nobs / precise parquet timestamp / qmd render duration -----
# These are already tracked, per source directory, by the existing
# _update_parquet_folder.R / _update_qmd_folder.R checkpoint mechanism
# (dirs.txt lists which source directories are under that pipeline) --
# read them here rather than recomputing anything (e.g. re-scanning
# parquet files for row counts), matching the standing rule that dataset
# sizes are only ever populated forward by the download/render hooks
# themselves, never bulk-backfilled from this script.
read_checkpoint <- function(dir_path, rdata_name, obj_name) {
  chk <- file.path(dir_path, rdata_name)
  if (!file.exists(chk)) return(NULL)
  e <- new.env()
  ok <- tryCatch({ load(chk, envir = e); TRUE }, error = function(err) FALSE)
  if (!ok || !exists(obj_name, envir = e)) return(NULL)
  get(obj_name, envir = e)
}

dirs_file <- file.path(root_dir, "_status", "dirs.txt")
tracked_dirs <- if (file.exists(dirs_file)) trimws(readLines(dirs_file)) else character()
tracked_dirs <- tracked_dirs[nzchar(tracked_dirs)]

parquet_info <- map_dfr(tracked_dirs, function(d) {
  x <- read_checkpoint(file.path(root_dir, d), "_update_parquet.RData", "update_parquet")
  if (is.null(x)) return(tibble())
  tibble(source = d, dataset = tools::file_path_sans_ext(x$filename),
         Nobs = x$nrow, parquet_updated = x$mtime)
})

qmd_info <- map_dfr(tracked_dirs, function(d) {
  x <- read_checkpoint(file.path(root_dir, d), "_update_qmd.RData", "update_qmd")
  if (is.null(x)) return(tibble())
  tibble(source = d, dataset = tools::file_path_sans_ext(x$filename),
         qmd_duration_sec = x$duration_sec, qmd_rendered_at = x$end)
})

if (length(complete_bases) == 0) {
  datasets <- tibble(
    source           = character(),
    dataset          = character(),
    Title            = character(),
    `.html`          = as.Date(character()),
    data_updated     = as.POSIXct(character()),
    Nobs             = integer(),
    qmd_duration_sec = double(),
    qmd_rendered_at  = as.POSIXct(character())
  )
  print(datasets)
} else {
  # Map each basepath to actual file paths for each extension
  paths_wide <- df %>%
    filter(basepath %in% complete_bases) %>%
    group_by(basepath, ext) %>%
    summarize(path = dplyr::first(path), .groups = "drop") %>%
    filter(ext %in% c(targets_core, targets_data)) %>%
    pivot_wider(names_from = ext, values_from = path)
  
  # Ensure expected columns exist even if entirely missing in this slice
  for (col in c("qmd", "html", "rdata", "parquet")) {
    if (!col %in% names(paths_wide)) paths_wide[[col]] <- NA_character_
  }
  
  # Derive rel_dir and dataset (stem), compute mtimes, and extract Title from .qmd
  datasets <- paths_wide %>%
    mutate(
      rel_dir = dplyr::coalesce(dirname(html), dirname(qmd), dirname(rdata), dirname(parquet)),
      rel_dir = {
        rd <- normalizePath(root_dir, winslash = "/", mustWork = FALSE)
        pr <- normalizePath(rel_dir, winslash = "/", mustWork = FALSE)
        out <- str_replace(pr, paste0("^", rd), "")
        out <- str_replace(out, "^/?", "")
        ifelse(out == "", ".", out)
      },
      dataset        = basename(basepath),
      html_mtime     = as.Date(file.info(html)$mtime),
      rdata_mtime    = as.Date(file.info(rdata)$mtime),
      parquet_mtime  = as.Date(file.info(parquet)$mtime),
      Title          = map_chr(qmd, get_qmd_title)
    ) %>%
    mutate(
      source = ifelse(rel_dir == ".", ".", strsplit(rel_dir, "/", fixed = TRUE) %>% map_chr(~ .x[1]))
    ) %>%
    transmute(
      source,
      dataset,   # <- basename/stem
      Title,     # <- from YAML front matter of the .qmd
      `.html`    = html_mtime,
      # Raw file-mtime date -- always computed, not just a fallback for
      # untracked sources. The checkpoint's parquet_updated can lag behind
      # the actual file if _update_parquet_folder.R hasn't been re-run
      # since the last real download (it isn't wired into every build
      # script), so pmax() below trusts whichever signal is more recent
      # instead of blindly preferring the checkpoint.
      .data_mtime = coalesce(parquet_mtime, rdata_mtime)
    ) %>%
    left_join(parquet_info, by = c("source", "dataset")) %>%
    left_join(qmd_info, by = c("source", "dataset")) %>%
    mutate(
      data_updated = pmax(parquet_updated, as.POSIXct(.data_mtime), na.rm = TRUE)
    ) %>%
    select(source, dataset, Title, `.html`, data_updated, Nobs,
           qmd_duration_sec, qmd_rendered_at) %>%
    arrange(source, dataset)
  
  print(datasets)
}

# --- Audience : colonnes visitors_90d / pageviews_90d / pageviews_all /
# rank_score, produites par data/_pageviews.R (GoatCounter + logs). Jointure
# souple : si _pageviews.RData n'existe pas encore, colonnes = NA (le tri
# par popularité retombe alors sur l'ordre alphabétique). On (re)génère
# _pageviews.RData d'abord si le script est présent.
if (file.exists("_pageviews.R")) {
  try(system2(file.path(R.home("bin"), "Rscript"),
              c("--vanilla", "-e", shQuote("source('_pageviews.R')")),
              stdout = "", stderr = ""), silent = TRUE)
}
if (file.exists("_pageviews.RData")) {
  pv_env <- new.env(); load("_pageviews.RData", envir = pv_env)
  datasets <- datasets %>%
    dplyr::left_join(
      pv_env$pageviews %>%
        dplyr::select(source, dataset, visitors_90d, pageviews_90d,
                      pageviews_all, rank_score),
      by = c("source", "dataset")
    )
} else {
  datasets$visitors_90d  <- NA_integer_
  datasets$pageviews_90d <- NA_integer_
  datasets$pageviews_all <- NA_integer_
  datasets$rank_score    <- NA_real_
}

save(datasets, file = "_datasets.RData")

# --- SEO / discovery artefacts -----------------------------------------
# Derived from the catalog we just saved, regenerated on every refresh:
#   code/_seo_search.R   -> data/search.json   (catalog search box)
#   code/_seo_sitemap.R  -> sitemap.xml + robots.txt   (repo root)
#   code/_seo_inject.R   -> schema.org/Dataset + <meta description> in every
#                           already-rendered data/<source>/<ds>.html
# search.json / sitemap.xml / robots.txt are committed and shipped as-is by
# rsync; the CI workflows re-run _seo_inject.R on the HTML they build.
Rscript_bin <- file.path(R.home("bin"), "Rscript")
try(system2(Rscript_bin, c("--vanilla", shQuote(here::here("code", "_seo_search.R")))))
try(system2(Rscript_bin, c("--vanilla", shQuote(here::here("code", "_seo_sitemap.R")))))
try(system2(Rscript_bin, c("--vanilla", shQuote(here::here("code", "_seo_inject.R")),
                           shQuote(path.expand(root_dir)))))


