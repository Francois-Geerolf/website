# Load packages --------

source(here::here("_rinit.R"))

# Folders to scan# Define the two sets
langs <- c("fr", "en")
folders <- c("publications", "public-debate", "working-papers")

# Create all combinations
roots <- as.vector(outer(folders, langs, paste, sep = "/"))
roots

# Helper: read file, collapse, strip links
read_qmd <- function(file) {
  read_file(file) %>%
    str_squish() %>%
    # remove [[...](...)] style links
    str_remove_all("\\[\\[[^\\]]*\\]\\([^\\)]*\\)\\]") %>%
    # remove normal markdown links [..](..)
    str_remove_all("\\[[^\\]]*\\]\\([^\\)]*\\)")
}

# Collect all .qmd files
qmd_files <- map(roots, ~list.files(.x, pattern = "\\.qmd$", full.names = TRUE, recursive = TRUE)) %>%
  unlist()

# Build tibble: Path + Content
bibliography <- tibble(
  File    = qmd_files,
  Content = map_chr(qmd_files, read_qmd)
)

bibliography

save(bibliography, file = "_bibliography.RData")

