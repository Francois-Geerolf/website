# Load packages --------
source(here::here("_rinit.R"))
setwd(here::here())

# Folders to scan# Define the two sets
langs <- c("fr", "en")
folders <- c("publications", "public-debate", "working-papers")

# Create all combinations
roots <- as.vector(outer(folders, langs, paste, sep = "/"))

# --- Core: allow one level of parentheses inside URLs -------------------------
# Captures the URL inside (...) while permitting single-level nested parentheses.
paren_url <- "\\(((?:[^()]|\\([^()]*\\))*)\\)"

#--- Helpers to extract icon-tagged links --------------------------------------

# PDF link (<i class="fas fa-file-pdf"></i>)
extract_pdf_link <- function(x) {
  pat <- paste0("(?is)\\[\\[\\s*<i[^>]*fa[- ]file[- ]pdf[^>]*>\\s*</i>[^\\]]*\\]\\s*", paren_url, "\\s*\\]")
  m <- str_match(x, pat)
  m[, 2]
}

# HTML link (<i class="fas fa-globe"></i>)
extract_html_link <- function(x) {
  pat <- paste0("(?is)\\[\\[\\s*<i[^>]*fa[- ]globe[^>]*>\\s*</i>[^\\]]*\\]\\s*", paren_url, "\\s*\\]")
  m <- str_match(x, pat)
  m[, 2]
}

# GitHub link (<i class="fab fa-github"></i>)
extract_github_link <- function(x) {
  pat <- paste0("(?is)\\[\\[\\s*<i[^>]*fa[^\">]*github[^>]*>\\s*</i>[^\\]]*\\]\\s*", paren_url, "\\s*\\]")
  m <- str_match(x, pat)
  m[, 2]
}

# Read, clean content (remove links), and extract icon links
parse_qmd <- function(file) {
  raw <- read_file(file)
  
  # Patterns to remove links, with parentheses-friendly URL group
  pat_icon_links   <- paste0("(?is)\\[\\[[^\\]]*\\]\\s*", paren_url, "\\s*\\]")
  pat_md_links     <- paste0("(?is)\\[[^\\]]*\\]\\s*", paren_url)
  
  tibble(
    File    = file,
    PDF     = extract_pdf_link(raw),
    HTML    = extract_html_link(raw),
    GitHub  = extract_github_link(raw),
    Content = raw %>%
      str_squish() %>%
      # remove [[...](...)] links (with tolerant URL)
      str_remove_all(pat_icon_links) %>%
      # remove single-bracket markdown links (with tolerant URL)
      str_remove_all(pat_md_links)
  )
}

#--- Collect files and build the tibble ----------------------------------------
cat("Collect files and build the tibble...\n")

qmd_files <- map(roots, ~list.files(.x, pattern = "\\.qmd$", full.names = TRUE, recursive = TRUE)) %>%
  unlist()

bibliography <- map_dfr(qmd_files, parse_qmd) %>%
  mutate(
    PDF   = if_else(!is.na(PDF)   & PDF   != "",
                    glue('<a href="{PDF}" target="_blank" rel="noopener noreferrer"><i class="fas fa-file-pdf"></i></a>'),
                    ""),
    HTML  = if_else(!is.na(HTML)  & HTML  != "",
                    glue('<a href="{HTML}" target="_blank" rel="noopener noreferrer"><i class="fas fa-globe"></i></a>'),
                    ""),
    GitHub= if_else(!is.na(GitHub)& GitHub!= "",
                    glue('<a href="{GitHub}" target="_blank" rel="noopener noreferrer"><i class="fab fa-github"></i></a>'),
                    "")
  ) %>%
  select(File, Content, PDF, HTML, GitHub) %>%
  # Arrange
  mutate(year = str_extract(File, "\\d{4}") %>% as.integer()) %>%
  arrange(desc(year))

#--- Save _bibliography.RData ----------------------------------------
cat("Save _bibliography.RData...\n")
save(bibliography, file = "_bibliography.RData")


