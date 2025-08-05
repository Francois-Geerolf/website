# Define the package list

packages <- c(
  "countrycode", "httr", "jsonlite",
  "ggplot2", "dplyr", "readr", "tibble", "stringr", "forcats", "purrr", "tidyr", "readxl",
  "rsdmx", "tools", "knitr", "scales", "viridis", "zoo",
  "lubridate", "ggrepel", "curl", "rmarkdown", "lubridate",
  "rvest", "gt", "quarto", "gtExtras"
)

# # Install any missing packages
# installed <- packages %in% rownames(installed.packages())
# if (any(!installed)) {
#   install.packages(packages[!installed], dependencies = TRUE)
# }

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))

# functions

ig_d <- function(source, dataset, file){
  i_g(paste0("data/", source, "/", dataset, "_files/figure-html/", file, "-1.png"))
}

i_g <- function(path) {
  new_path <- gsub("https://fgeerolf.com/", "", path)
  new_path_pdf <- paste0(gsub(".png", "", new_path), ".pdf")
  if (interactive()) {
    if (file.exists(paste0("~/iCloud/website/", new_path))) {
      return(knitr::include_graphics(paste0("~/iCloud/website/", new_path)))
    } else{
      return(knitr::include_graphics(paste0("~/iCloud/", new_path)))
    }
  } else if (knitr::is_latex_output()) {
    if (file.exists(paste0("~/iCloud/website/", new_path_pdf))) {
      return(knitr::include_graphics(paste0("~/iCloud/website/", new_path_pdf)))
    } else if (file.exists(paste0("~/iCloud/website/", new_path))){
      return(knitr::include_graphics(paste0("~/iCloud/website/", new_path)))
    } else {
      return(knitr::include_graphics(paste0("~/iCloud/", new_path)))
    }
  }
  else {
    return(knitr::include_graphics(paste0("https://fgeerolf.com/", new_path)))
  }
}

ig_b <- function(source = "", folder = "", folder2 = "", folder3 = "", file = "", ext = ".png") {
  # Build path components conditionally
  parts <- c("bib", source, folder, folder2, folder3, file)
  # Remove empty strings
  path <- paste(parts[parts != ""], collapse = "/")
  # Add file extension only if not already present
  if (!grepl(paste0("\\", ext, "$"), path)) {
    path <- paste0(path, ext)
  }
  i_g(path)
}