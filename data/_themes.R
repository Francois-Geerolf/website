source(here::here("_rinit.R"))

# Set your root folder here
setwd(here::here("data"))


themes <- tibble::tibble(
  theme = tools::file_path_sans_ext(
    list.files(path = root_dir, pattern = "\\.qmd$", full.names = FALSE, ignore.case = TRUE)
  )
) |>
  dplyr::filter(
    !theme %in% c("api", "index", "themes"),
    !grepl("_update", theme, perl = TRUE)
  ) |>
  dplyr::arrange(theme) %>%
  mutate(Title = read_lines(paste0(theme, ".qmd"), skip = 1, n_max = 1) %>% gsub("title: ", "", .) %>% gsub("\"", "", .)) %>%
  mutate(Link = glue::glue("[Link](https://fgeerolf.com/data/{theme}.html)"))

save(themes, file = "_themes.RData")


