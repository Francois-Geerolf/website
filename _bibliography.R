# Load packages --------
source(here::here("_rinit.R"))
setwd(here::here())

# Folders to scan# Define the two sets
langs <- c("fr", "en")
folders <- c("publications", "public-debate", "working-papers", "presentations", "press")

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

# Parse date time from content in  ----------------

extract_pub_date <- function(x) {
  
  x <- str_squish(x)
  
  # Mois FR / EN
  months_fr <- c("janvier","février","fevrier","mars","avril","mai","juin",
                 "juillet","août","aout","septembre","octobre","novembre","décembre","decembre")
  months_en <- c("january","february","march","april","may","june","july",
                 "august","september","october","november","december")
  
  m_fr <- paste(months_fr, collapse = "|")
  m_en <- paste(months_en, collapse = "|")
  
  # Patterns
  pat_fr <- regex(paste0("\\b(\\d{1,2}\\s+)?(", m_fr, ")\\s+(\\d{4})\\b"), ignore_case = TRUE)
  pat_en <- regex(paste0("\\b(\\d{1,2}\\s+)?(", m_en, ")\\s+(\\d{4})\\b"), ignore_case = TRUE)
  pat_year <- regex("\\b(19|20)\\d{2}\\b")
  
  hit_fr   <- str_extract(x, pat_fr)
  hit_en   <- str_extract(x, pat_en)
  hit_year <- str_extract(x, pat_year)
  
  # Priorité : mois+année > année seule
  hit <- coalesce(hit_fr, hit_en)
  
  # ---- Cas 1 & 2 : mois + année (avec ou sans jour)
  if (!is.na(hit)) {
    
    hit2 <- if_else(
      str_detect(hit, "^\\d{1,2}\\s"),
      hit,
      paste("1", hit)
    )
    
    return(
      parse_date_time(
        hit2,
        orders = "d B Y",
        locale = if (!is.na(hit_fr)) "fr_FR" else "en_US"
      ) |>
        as_date()
    )
  }
  
  # ---- Cas 3 : année seule
  if (!is.na(hit_year)) {
    return(as.Date(paste0(hit_year, "-01-01")))
  }
  
  # ---- Sinon
  as.Date(NA)
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
  # Parse using function extract_pub_date
  mutate(date = as.Date(unlist(map(Content, extract_pub_date)))) %>%
  arrange(desc(date)) %>%
  select(File, Content, date, PDF, HTML, GitHub)

#--- Save _bibliography.RData ----------------------------------------
cat("Save _bibliography.RData...\n")
save(bibliography, file = "_bibliography.RData")


