source(here::here("_rinit.R"))
setwd(here::here("data"))

# Set your root folder here
root_dir <- "~/iCloud/website/data"

# Required extensions (case-insensitive)
targets <- c("qmd", "html", "rdata")

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
    # Find the first closing fence after start
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
    # strip surrounding single/double quotes if present
    raw <- sub('^"(.*)"$', "\\1", raw)
    raw <- sub("^'(.*)'$", "\\1", raw)
    return(raw)
  }
  
  NA_character_
}


# List candidate files recursively, ignoring case on pattern
files <- list.files(
  root_dir,
  pattern = "\\.(qmd|html|rdata)$",
  ignore.case = TRUE,
  recursive = TRUE,
  full.names = TRUE
)

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

# Identify basenames that have ALL required extensions present
complete_bases <- df %>%
  group_by(basepath) %>%
  summarize(have_ext = list(sort(unique(ext))), .groups = "drop") %>%
  filter(map_lgl(have_ext, ~ all(targets %in% .x))) %>%
  pull(basepath)

if (length(complete_bases) == 0) {
  datasets <- tibble(
    source  = character(),
    dataset = character(),
    Title   = character(),
    `.html` = as.POSIXct(character()),
    `.RData`= as.POSIXct(character())
  )
  print(datasets)
} else {
  # Map each basepath to actual file paths for each extension
  paths_wide <- df %>%
    filter(basepath %in% complete_bases) %>%
    group_by(basepath, ext) %>%
    summarize(path = dplyr::first(path), .groups = "drop") %>%
    filter(ext %in% targets) %>%
    pivot_wider(names_from = ext, values_from = path)
  
  # Derive rel_dir and dataset (stem), compute mtimes, and extract Title from .qmd
  datasets <- paths_wide %>%
    mutate(
      rel_dir = dplyr::coalesce(dirname(html), dirname(qmd), dirname(rdata)),
      rel_dir = {
        rd <- normalizePath(root_dir, winslash = "/", mustWork = FALSE)
        pr <- normalizePath(rel_dir, winslash = "/", mustWork = FALSE)
        out <- str_replace(pr, paste0("^", rd), "")
        out <- str_replace(out, "^/?", "")
        ifelse(out == "", ".", out)
      },
      dataset = basename(basepath),
      html_mtime  = as.Date(file.info(html)$mtime),
      rdata_mtime = as.Date(file.info(rdata)$mtime),
      Title = map_chr(qmd, get_qmd_title)
    ) %>%
    mutate(
      source = ifelse(rel_dir == ".", ".", strsplit(rel_dir, "/", fixed = TRUE) %>% map_chr(~ .x[1]))
    ) %>%
    transmute(
      source,
      dataset,   # <- basename/stem
      Title,     # <- from YAML front matter of the .qmd
      `.html`  = html_mtime,
      `.RData` = rdata_mtime
    ) %>%
    arrange(source, dataset)
  
  print(datasets)
}


save(datasets, file = "_datasets.RData")

