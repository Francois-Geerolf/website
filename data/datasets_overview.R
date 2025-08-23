library(dplyr)
library(glue)
library(gt)

load("_datasets.RData")


# Quarto header
header <- c(
  "---",
  "title: \"Datasets Overview\"",
  "subtitle: Data",
  "lang: en",
  "name: François Geerolf",
  "url: https://fgeerolf.com",
  "format: html",
  "toc: true",
  "theme: flatly",
  "embed-resources: false",
  "editor: source",
  "---",
  "",
  "```{r}",
  "library(dplyr)",
  "library(gt)",
  "source(here::here(\"_rinit.R\"))",
  "```\n\n"
)


# For each dataset, create a section
sections <- datasets %>%
  arrange(source) %>%
  pull(source) %>%
  unique() %>%
  map(function(ds) {
    glue(
      "## {ds}\n\n",
      "```{{r}}\n",
      "datasets %>%\n",
      "  filter(source == \"{ds}\") %>%\n",
      "  source_dataset_file_updates2()\n",
      "```\n\n"
    )
  })

# Combine everything
qmd_content <- c(header, unlist(sections))

# Write to file
writeLines(qmd_content, "datasets_overview.qmd")
