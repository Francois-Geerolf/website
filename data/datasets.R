source(here::here("_rinit.R"))
setwd(here::here("data"))
load("_datasets.RData")

# Quarto header
header <- c(
  "---",
  "title: \"Datasets Overview\"",
  "subtitle: Data",
  "lang: en",
  "author:",
  "  - name: François Geerolf",
  "    url: https://fgeerolf.com",
  "format: html",
  "toc: true",
  "theme: flatly",
  "embed-resources: false",
  "editor: source",
  "---",
  "",
  "```{r, echo = F, warnings = F}",
  "source(here::here(\"_rinit.R\"))",
  "knitr::opts_chunk$set(echo = F)",
  "```\n\n"
)


# For each dataset, create a section
sections <- datasets %>%
  arrange(source) %>%
  pull(source) %>%
  unique() %>%
  map(function(ds) {
    glue(
      "# {ds}\n\n",
      "```{{r}}\n",
      "datasets %>%\n",
      "  filter(source == \"{ds}\") %>%\n",
      "  source_dataset_file_updates2()\n",
      "```\n\n"
    )
  })

footer <- c(
  "# All\n",
  "```{r}",
  "datasets %>%",
  "  source_dataset_file_updates3()",
  "```\n\n"
)

# Combine everything
qmd_content <- c(header, unlist(sections), footer)

# Write to file
writeLines(qmd_content, "datasets.qmd")
