# Load packages --------

source(here::here("_packages.R"))

load(here::here("data", "_datasets.RData"))

# # Install any missing packages
# installed <- packages %in% rownames(installed.packages())
# if (any(!installed)) {
#   install.packages(packages[!installed], dependencies = TRUE)
# }

# Load all packages# Load all packages silently
invisible(
  suppressMessages(
    suppressWarnings(
      lapply(packages, library, character.only = TRUE)
    )
  )
)

# Echo = T
knitr::opts_chunk$set(echo = T)

# functions -----

ig_r <-function(paper, file) i_g(paste0("replications/", paper, "_files/figure-html/", file, "-1.png"))


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

add_flags <- ggimage::geom_image(
  data = function(df) {
    # Guard empty/invalid
    if (is.null(df) || nrow(df) == 0) return(df[0, , drop = FALSE])
    df <- tibble::as_tibble(df) %>% dplyr::ungroup()
    
    # --- Find the y/value column (first match wins)
    y_candidates <- c("obsValue", "OBS_VALUE", "values", "value")
    y_col <- y_candidates[y_candidates %in% names(df)][1]
    
    # --- Find the area column (Ref_area, Geo, country, etc.)
    area_candidates <- c(
      "Ref_area","ref_area",
      "Geo","geo","GEO",
      "country","Country","COUNTRY", "Location"
    )
    area_col <- area_candidates[area_candidates %in% names(df)][1]
    
    # Need: date, area, and a y column
    if (is.na(y_col) || is.na(area_col) || !"date" %in% names(df))
      return(df[0, , drop = FALSE])
    
    # Standardize to .y__ and .area__
    df <- df %>%
      dplyr::mutate(
        .y__ = .data[[y_col]],
        .area__ = as.character(.data[[area_col]])
      )
    
    # Largest group size across dates
    n_max <- max(dplyr::count(df, date, name = "n")$n, na.rm = TRUE)
    if (!is.finite(n_max) || n_max < 1) return(df[0, , drop = FALSE])
    
    # Pick the date with the best separation in y (to avoid overlaps)
    per_date <- df %>%
      dplyr::group_by(date) %>%
      dplyr::filter(dplyr::n() == n_max) %>%
      dplyr::arrange(.y__, .by_group = TRUE) %>%
      dplyr::summarise(
        dist = if (n_max > 1) min(diff(.y__), na.rm = TRUE) else Inf,
        .groups = "drop"
      )
    if (nrow(per_date) == 0) return(df[0, , drop = FALSE])
    
    best_date <- per_date %>%
      dplyr::arrange(dplyr::desc(dist), date) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::pull(date)
    
    # Helper: slugify area to file name
    slugify <- function(x) {
      # transliterate accents to ASCII where possible
      x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
      x <- tolower(x)
      x <- gsub("'", "", x)                 # drop apostrophes
      x <- gsub("[^a-z0-9]+", "-", x)       # non-alnum -> hyphen
      x <- gsub("^-+|-+$", "", x)           # trim hyphens
      x
    }
    
    # Keep rows from that best date and build image path from area
    df %>%
      dplyr::filter(date %in% best_date) %>%
      dplyr::arrange(.y__) %>%
      dplyr::mutate(
        image = file.path(
          "../../icon/flag/round",
          paste0(slugify(.area__), ".png")
        )
      )
  },
  mapping = ggplot2::aes(x = date, y = .y__, image = image),
  asp = 1.5,
  inherit.aes = FALSE
)

print_table_long <- . %>%
  gt::gt() %>%
  gt::fmt_markdown(., columns = one_of(c("source", "dataset", "theme"))) %>%
  gt::cols_align(align = "center", columns = everything()) %>% 
  gt::tab_options(column_labels.font.weight = "bold")

print_table <- . %>%
  knitr::kable(align = "c", booktabs = T, linesep = "", longtable = T, escape = F) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            latex_options = c("striped", "hold_position", "repeat_header"))

print_table2 <- . %>%
  gt::gt() %>%
  gt::cols_align(align = "center", columns = everything()) %>% 
  gt::fmt_markdown(., columns = one_of(c("source", "dataset", "theme", "id"))) %>%
  gt::tab_options(column_labels.font.weight = "bold")


print_table_no_escape <- . %>%
  knitr::kable(align = "c", booktabs = T, linesep = "", longtable = T, escape = F) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            latex_options = c("striped", "hold_position", "repeat_header"))


print_table_noname <- . %>%
  knitr::kable(align = "c", booktabs = T, linesep = "", longtable = T, col.names = NULL) %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                            latex_options = c("striped", "hold_position", "repeat_header"))

source_dataset_file_updates <- . %>%
  left_join(datasets, by = c("source", "dataset")) %>%
  mutate(dataset = glue::glue("[{dataset}](https://fgeerolf.com/data/{source}/{dataset}.html)"),
         dataset = map(dataset, gt::md),
         source = glue::glue("[{source}](https://fgeerolf.com/data/{source})"),
         source = map(source, gt::md)) %>%
  gt::gt() %>%
  gt::cols_align(align = "center", columns = everything()) %>% 
  gt::tab_options(column_labels.font.weight = "bold")


source_dataset_file_updates2 <- . %>%
  mutate(dataset = glue::glue("[{dataset}](https://fgeerolf.com/data/{source}/{dataset}.html)"),
         dataset = map(dataset, gt::md)) %>%
  select(-source) %>%
  gt::gt() %>%
  gt::cols_align(align = "center", columns = everything()) %>% 
  gt::tab_options(column_labels.font.weight = "bold")


source_dataset_file_updates3 <- . %>%
  mutate(dataset = glue::glue("[{dataset}](https://fgeerolf.com/data/{source}/{dataset}.html)"),
         source = glue::glue("[{source}](https://fgeerolf.com/data/{source})")) %>%
  gt::gt() %>%
  fmt_markdown(columns = c("dataset", "source")) %>%
  gt::cols_align(align = "center", columns = everything()) %>% 
  gt::tab_options(column_labels.font.weight = "bold") %>% 
  # Interactive gt
  gt::opt_interactive(use_search = T,
                      use_pagination = T,
                      use_pagination_info = F,
                      use_compact_mode = T,
                      use_resizers = TRUE,
                      page_size_default = 20,
                      page_size_values = c(5, 10, 15, 20))


source_dataset_file_updates4 <- . %>%
  mutate(dataset = glue::glue("[{dataset}](https://fgeerolf.com/data/{source}/{dataset}.html)"),
         source = glue::glue("[{source}](https://fgeerolf.com/data/{source})")) %>%
  gt::gt() %>%
  fmt_markdown(columns = c("dataset", "source")) %>%
  gt::cols_align(align = "center", columns = everything()) %>% 
  gt::tab_options(column_labels.font.weight = "bold") %>% 
  # Make the 3rd column wider
  gt::cols_width(
    1 ~ gt::px(90),
    2 ~ gt::px(120),
    3 ~ gt::px(400),        # make the 3rd column much larger
    4 ~ gt::px(90),
    5 ~ gt::px(90),
    gt::everything() ~ gt::px(120)  # optional: set a default for others
  ) %>%
  # Interactive gt
  gt::opt_interactive(use_search = T,
                      use_pagination = T,
                      use_pagination_info = F,
                      use_compact_mode = T,
                      use_resizers = TRUE,
                      page_size_default = 20,
                      page_size_values = c(5, 10, 15, 20))

source_dataset_title_file_updates <- . %>%
  mutate(type = map(source, ~ tibble(type = c(".RData", ".html")))) %>%
  unnest %>%
  mutate(date = paste0("~/iCloud/website/data/", source, "/", dataset, type) %>% file.info %>% pluck("mtime") %>% as.Date()) %>%
  spread(type, date) %>%
  mutate(Title = read_lines(paste0("~/iCloud/website/data/", source, "/",dataset, ".qmd"), skip = 1, n_max = 1) %>% gsub("title: ", "", .) %>% gsub("\"", "", .)) %>%
  select(Title, everything()) %>%
  mutate(dataset = glue::glue("[{dataset}](https://fgeerolf.com/data/{source}/{dataset}.html)"),
         dataset = map(dataset, gt::md),
         source = glue::glue("[{source}](https://fgeerolf.com/data/{source})"),
         source = map(source, gt::md)) %>%
  gt::gt() %>%
  gt::cols_align(align = "center", columns = everything()) %>% 
  gt::tab_options(column_labels.font.weight = "bold")

theme_file_updates <- . %>%
  mutate(type = map(theme, ~ tibble(type = c(".html")))) %>%
  unnest %>%
  mutate(date = paste0("~/iCloud/website/data/", theme, type) %>% file.info %>% pluck("mtime") %>% as.Date()) %>%
  spread(type, date) %>%
  mutate(Title = read_lines(paste0("~/iCloud/website/data/", theme, ".qmd"), skip = 1, n_max = 1) %>% gsub("title: ", "", .) %>% gsub("\"", "", .)) %>%
  select(theme, Title, everything()) %>%
  mutate(theme = glue::glue("[{theme}](https://fgeerolf.com/data/{theme}.html)"),
         theme = map(theme, gt::md)) %>%
  gt::gt() %>%
  gt::cols_align(align = "center", columns = everything()) %>% 
  gt::tab_options(column_labels.font.weight = "bold")



print_table_conditional <- function(data){
  if (dim(data)[1] > 30){
    if (knitr::is_html_output()) DT::datatable(data, filter = 'top', rownames = F) else data
  } else{
    if (knitr::is_html_output()) print_table(data) else data
  }
}

print_table_conditional2 <- function(data){
  if (dim(data)[1] > 30){
    if (knitr::is_html_output()) DT::datatable(data, filter = 'top', rownames = F) else data
  } else{
    if (knitr::is_html_output()) print_table2(data) else data
  }
}



print_table_conditional_10 <- function(data){
  if (dim(data)[1] > 10){
    if (knitr::is_html_output()) DT::datatable(data, filter = 'top', rownames = F) else data
  } else{
    if (knitr::is_html_output()) print_table(data) else data
  }
}

print_table_conditional_20 <- function(data){
  if (dim(data)[1] > 20){
    if (knitr::is_html_output()) DT::datatable(data, filter = 'top', rownames = F) else data
  } else{
    if (knitr::is_html_output()) print_table(data) else data
  }
}

print_table_conditional_30 <- function(data){
  if (dim(data)[1] > 30){
    if (knitr::is_html_output()) DT::datatable(data, filter = 'top', rownames = F) else data
  } else{
    if (knitr::is_html_output()) print_table(data) else data
  }
}

print_table_conditional_100 <- function(data){
  if (dim(data)[1] > 100){
    if (knitr::is_html_output()) DT::datatable(data, filter = 'top', rownames = F) else data
  } else{
    if (knitr::is_html_output()) print_table(data) else data
  }
}



metadata_load <- function(code, CL_code, data = QNA_EXPENDITURE_CAPITA_var){
  assign(code, as.data.frame(data@codelists, codelistId = CL_code) %>%
           select(id, label.en) %>%
           setNames(c(code, str_to_title(code))),
         envir = .GlobalEnv)
}

metadata_load_fr <- function(code, CL_code, data = QNA_EXPENDITURE_CAPITA_var){
  assign(code, as.data.frame(data@codelists, codelistId = CL_code) %>%
           select(id, label.fr) %>%
           setNames(c(code, str_to_title(code))),
         envir = .GlobalEnv)
}

code_names <- function(data = QNA_EXPENDITURE_CAPITA_var){
  assign("code_names",
         data@codelists@codelists %>%
           vapply(., function(x) x@id, "character") %>%
           as_tibble %>%
           arrange(value) %>%
           mutate(empty = 1) %>%
           spread(value, empty),
         envir = .GlobalEnv)
}



code_names_unique <- function(data = QNA_EXPENDITURE_CAPITA_var){
  assign("code_names_unique",
         data %>%
           select(-obsTime, -obsValue) %>%
           select_if(~ n_distinct(.) > 1) %>%
           names(.) %>%
           as_tibble %>%
           arrange(value) %>%
           mutate(empty = 1) %>%
           spread(value, empty),
         envir = .GlobalEnv)
}


obsTime_FREQUENCY_to_date <- function(obsTime, FREQUENCY){
  if (FREQUENCY == "A"){
    date = paste0(obsTime, "-01-01") %>% as.Date
  } else if (FREQUENCY == "Q"){
    year = obsTime %>% substr(1, 4)
    qtr = obsTime %>% substr(7, 7) %>% as.numeric
    month = (qtr - 1)*3 + 1
    month = month %>% str_pad(., 2, pad = "0")
    date = paste0(year, "-", month, "-01") %>% as.Date
  } else if (FREQUENCY == "M"){
    date = paste0(obsTime, "-01-01") %>% as.Date
  }
  return(date)
}

frequency_to_date <- function(data){
  data %>%
    rowwise() %>%
    mutate(date = obsTime_FREQUENCY_to_date(obsTime, FREQUENCY)) %>%
    select(-obsTime) %>%
    select(date, everything())
}

quarter_to_date <- function(data){
  data %>%
    mutate(year = obsTime %>% substr(1, 4),
           qtr = obsTime %>% substr(7, 7) %>% as.numeric,
           month = (qtr - 1)*3 + 1,
           month = month %>% str_pad(., 2, pad = "0"),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -qtr, -month, -obsTime) %>%
    select(date, everything())
}


quarter_to_enddate <- function(data){
  data %>%
    mutate(year = obsTime %>% substr(1, 4),
           qtr = obsTime %>% substr(7, 7) %>% as.numeric,
           month = (qtr)*3,
           month = month %>% str_pad(., 2, pad = "0"),
           date = as.Date(paste0(year, "-", month, "-01")),
           date = date + months(1) - days(1)) %>%
    select(-year, -qtr, -month, -obsTime) %>%
    select(date, everything())
}



quarter_to_date2 <- function(data){
  data %>%
    mutate(year = yearqtr %>% substr(1, 4),
           qtr = yearqtr %>% substr(6, 6) %>% as.numeric,
           month = (qtr - 1)*3 + 1,
           month = month %>% str_pad(., 2, pad = "0"),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -qtr, -month, -yearqtr) %>%
    select(date, everything())
}


month_to_date <- function(data){
  data %>%
    mutate(date = paste0(obsTime, "-01") %>% as.Date) %>%
    select(-obsTime) %>%
    select(date, everything())
}


year_to_date <- function(data){
  data %>%
    mutate(date = paste0(obsTime, "-01-01") %>% as.Date) %>%
    select(-obsTime) %>%
    select(date, everything())
}


year_to_enddate <- function(data){
  data %>%
    mutate(date = paste0(obsTime, "-12-31") %>% as.Date) %>%
    select(-obsTime) %>%
    select(date, everything())
}

yearqtr_to_date <- function(data){
  data %>%
    mutate(year = yearqtr %>% floor,
           month = (yearqtr - year)*12 + 1,
           month = str_pad(month, 2, pad = 0),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -month, -yearqtr) %>%
    select(date, everything())
}

yearqtr_to_enddate <- function(data){
  data %>%
    mutate(year = yearqtr %>% floor,
           month = (yearqtr - year)*12 + 1,
           month = str_pad(month, 2, pad = 0),
           date = as.Date(paste0(year, "-", month, "-01")) + months(1) - day(1)) %>%
    select(-year, -month, -yearqtr) %>%
    select(date, everything())
}



yearqtr_to_date2 <- function(data){
  data %>%
    mutate(year = yearqtr %>% floor,
           month = (yearqtr - year)*12 + 1,
           month = str_pad(month, 2, pad = 0),
           date = paste0(year, "-", month, "-01") %>% as.Date) %>%
    select(-year, -month) %>%
    select(date, everything())
}

