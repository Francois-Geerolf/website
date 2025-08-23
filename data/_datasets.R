source(here::here("_rinit.R"))
setwd(here::here("data"))

# Set your root folder here
root_dir <- "~/iCloud/website/data"

# Extensions we require (case-insensitive)
targets <- c("qmd", "html", "rdata")

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
    fname = basename(path),
    ext_raw = tools::file_ext(fname),
    ext = tolower(ext_raw),
    stem = tools::file_path_sans_ext(fname),
    basepath = file.path(rel_dir, stem)
  )

# Keep only basenames that have all required extensions
complete_bases <- df %>%
  group_by(basepath) %>%
  summarize(have_ext = list(sort(unique(ext))), .groups = "drop") %>%
  filter(map_lgl(have_ext, ~ all(targets %in% .x))) %>%
  pull(basepath)

if (length(complete_bases) == 0) {
  datasets <- tibble(
    source = character(),
    dataset = character(),
    `.html` = as.POSIXct(character()),
    `.RData` = as.POSIXct(character())
  )
  print(datasets)
} else {
  # Map each basepath to the actual path for each required extension
  paths_wide <- df %>%
    filter(basepath %in% complete_bases) %>%
    group_by(basepath, ext) %>%
    summarize(path = dplyr::first(path), .groups = "drop") %>%
    filter(ext %in% targets) %>%
    pivot_wider(names_from = ext, values_from = path)
  
  # Derive rel_dir from one of the files (prefer html)
  paths_wide <- paths_wide %>%
    mutate(
      rel_dir = dplyr::coalesce(dirname(html), dirname(qmd), dirname(rdata)),
      rel_dir = {
        rd <- normalizePath(root_dir, winslash = "/", mustWork = FALSE)
        pr <- normalizePath(rel_dir, winslash = "/", mustWork = FALSE)
        out <- str_replace(pr, paste0("^", rd), "")
        out <- str_replace(out, "^/?", "")
        ifelse(out == "", ".", out)
      },
      dataset = basename(basepath)  # <- use stem as dataset
    )
  
  datasets <- paths_wide %>%
    mutate(
      html_mtime  = as.Date(file.info(html)$mtime),
      rdata_mtime = as.Date(file.info(rdata)$mtime)
    ) %>%
    mutate(
      source = ifelse(rel_dir == ".", ".", strsplit(rel_dir, "/", fixed = TRUE) %>% map_chr(~ .x[1]))
    ) %>%
    transmute(
      source,
      dataset,
      `.html`  = html_mtime,
      `.RData` = rdata_mtime
    ) %>%
    arrange(source, dataset)
  
  print(datasets)
}

save(datasets, file = "_datasets.RData")

