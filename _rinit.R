# Define the package list

packages <- c(
  "ggimage", "countrycode", "httr", "gt", "gtExtras",
  "ggplot2", "dplyr", "tidyr", "readr",
  "purrr", "tibble", "stringr", "forcats",
  "quarto", "readxl", "rsdmx", "tools",
  "knitr", "scales", "viridis", "zoo", "lubridate",
  "ggrepel", "curl", "rmarkdown", "rvest", "jsonlite",
  "here", "i18n", "r2country", "eurostat", "textcat"
)

# # Install any missing packages
# installed <- packages %in% rownames(installed.packages())
# if (any(!installed)) {
#   install.packages(packages[!installed], dependencies = TRUE)
# }

# Load all packages
invisible(lapply(packages, library, character.only = TRUE))

knitr::opts_chunk$set(echo = T)

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


add_flags <- function(number = 2){
  if (number == 2){
    geom_image(data = . %>%
                 group_by(date) %>%
                 filter(n() == 2) %>%
                 arrange(values) %>%
                 mutate(dist = values[2]-values[1]) %>%
                 arrange(-dist, date) %>%
                 head(2) %>%
                 mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
               aes(x = date, y = values, image = image), asp = 1.5)
  } else if (number == 3){
    geom_image(data = . %>%
                 group_by(date) %>%
                 filter(n() == 3) %>%
                 arrange(values) %>%
                 mutate(dist = min(values[2]-values[1],values[3]-values[2])) %>%
                 arrange(-dist, date) %>%
                 head(3) %>%
                 mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
               aes(x = date, y = values, image = image), asp = 1.5)
  } else if (number == 4) {
    geom_image(data = . %>%
                 group_by(date) %>%
                 filter(n() == 4) %>%
                 arrange(values) %>%
                 mutate(dist = min(values[2]-values[1],values[3]-values[2],values[4]-values[3])) %>%
                 arrange(-dist, date) %>%
                 head(4) %>%
                 mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
               aes(x = date, y = values, image = image), asp = 1.5)
  } else if (number == 5) {
    geom_image(data = . %>%
                 group_by(date) %>%
                 filter(n() == 5) %>%
                 arrange(values) %>%
                 mutate(dist = min(values[2]-values[1],values[3]-values[2],values[4]-values[3],
                                   values[5]-values[4])) %>%
                 arrange(-dist, date) %>%
                 head(5) %>%
                 mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
               aes(x = date, y = values, image = image), asp = 1.5)
  } else if (number == 6) {
    geom_image(data = . %>%
                 group_by(date) %>%
                 filter(n() == 6) %>%
                 arrange(values) %>%
                 mutate(dist = min(values[2]-values[1],values[3]-values[2],values[4]-values[3],
                                   values[5]-values[4],values[6]-values[5])) %>%
                 arrange(-dist, date) %>%
                 head(6) %>%
                 mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
               aes(x = date, y = values, image = image), asp = 1.5)
  } else if (number == 7) {
    geom_image(data = . %>%
                 group_by(date) %>%
                 filter(n() == 7) %>%
                 arrange(values) %>%
                 mutate(dist = min(values[2]-values[1],values[3]-values[2],values[4]-values[3],
                                   values[5]-values[4],values[6]-values[5],values[7]-values[6])) %>%
                 arrange(-dist, date) %>%
                 head(7) %>%
                 mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
               aes(x = date, y = values, image = image), asp = 1.5)
  } else if (number == 8) {
    geom_image(data = . %>%
                 group_by(date) %>%
                 filter(n() == 8) %>%
                 arrange(values) %>%
                 mutate(dist = min(values[2]-values[1],values[3]-values[2],values[4]-values[3],
                                   values[5]-values[4],values[6]-values[5],values[7]-values[6],
                                   values[8]-values[7])) %>%
                 arrange(-dist, date) %>%
                 head(8) %>%
                 mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
               aes(x = date, y = values, image = image), asp = 1.5)
  } else if (number == 9) {
    geom_image(data = . %>%
                 group_by(date) %>%
                 filter(n() == 9) %>%
                 arrange(values) %>%
                 mutate(dist = min(values[2]-values[1],values[3]-values[2],values[4]-values[3],
                                   values[5]-values[4],values[6]-values[5],values[7]-values[6],
                                   values[8]-values[7],values[9]-values[8])) %>%
                 arrange(-dist, date) %>%
                 head(9) %>%
                 mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
               aes(x = date, y = values, image = image), asp = 1.5)
  } else if (number == 10) {
    geom_image(data = . %>%
                 group_by(date) %>%
                 filter(n() == 10) %>%
                 arrange(values) %>%
                 mutate(dist = min(values[2]-values[1],values[3]-values[2],values[4]-values[3],
                                   values[5]-values[4],values[6]-values[5],values[7]-values[6],
                                   values[8]-values[7],values[9]-values[8],values[10]-values[9])) %>%
                 arrange(-dist, date) %>%
                 head(10) %>%
                 mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
               aes(x = date, y = values, image = image), asp = 1.5)
  } else{
    warning("Provide an integer between 1 and 10")
  }
}


add_2flags <- geom_image(data = . %>%
                           group_by(date) %>%
                           filter(n() == 2) %>%
                           arrange(values) %>%
                           mutate(dist = values[2]-values[1]) %>%
                           arrange(-dist, date) %>%
                           head(2) %>%
                           mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
                         aes(x = date, y = values, image = image), asp = 1.5)

add_3flags <- geom_image(data = . %>%
                           group_by(date) %>%
                           filter(n() == 3) %>%
                           arrange(values) %>%
                           mutate(dist = min(values[2]-values[1],values[3]-values[2])) %>%
                           arrange(-dist, date) %>%
                           head(3) %>%
                           mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
                         aes(x = date, y = values, image = image), asp = 1.5)


add_4flags <- geom_image(data = . %>%
                           group_by(date) %>%
                           filter(n() == 4) %>%
                           arrange(values) %>%
                           mutate(dist = min(values[2]-values[1],values[3]-values[2],values[4]-values[3])) %>%
                           arrange(-dist, date) %>%
                           head(4) %>%
                           mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
                         aes(x = date, y = values, image = image), asp = 1.5)


add_5flags <- geom_image(data = . %>%
                           group_by(date) %>%
                           filter(n() == 5) %>%
                           arrange(values) %>%
                           mutate(dist = min(values[2]-values[1],values[3]-values[2],values[4]-values[3],
                                             values[5]-values[4])) %>%
                           arrange(-dist, date) %>%
                           head(5) %>%
                           mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
                         aes(x = date, y = values, image = image), asp = 1.5)


add_6flags <- geom_image(data = . %>%
                           group_by(date) %>%
                           filter(n() == 6) %>%
                           arrange(values) %>%
                           mutate(dist = min(values[2]-values[1],values[3]-values[2],values[4]-values[3],
                                             values[5]-values[4],values[6]-values[5])) %>%
                           arrange(-dist, date) %>%
                           head(6) %>%
                           mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
                         aes(x = date, y = values, image = image), asp = 1.5)


add_7flags <- geom_image(data = . %>%
                           group_by(date) %>%
                           filter(n() == 7) %>%
                           arrange(values) %>%
                           mutate(dist = min(values[2]-values[1],values[3]-values[2],values[4]-values[3],
                                             values[5]-values[4],values[6]-values[5],values[7]-values[6])) %>%
                           arrange(-dist, date) %>%
                           head(7) %>%
                           mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
                         aes(x = date, y = values, image = image), asp = 1.5)


add_8flags <- geom_image(data = . %>%
                           group_by(date) %>%
                           filter(n() == 8) %>%
                           arrange(values) %>%
                           mutate(dist = min(values[2]-values[1],values[3]-values[2],values[4]-values[3],
                                             values[5]-values[4],values[6]-values[5],values[7]-values[6],
                                             values[8]-values[7])) %>%
                           arrange(-dist, date) %>%
                           head(8) %>%
                           mutate(image = paste0("../../icon/flag/round/", str_to_lower(gsub(" ", "-", Geo)), ".png")),
                         aes(x = date, y = values, image = image), asp = 1.5)

