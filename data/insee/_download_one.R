#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
  library(readr)
  library(glue)
  library(fs)
  library(zoo)
  library(rsdmx)
})

# --- CLI args ---------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
default_datasets <- "IPCH-2015"
datasets <- if (length(args) == 0) default_datasets else args

message("Datasets to fetch: ", paste(datasets, collapse = ", "))

# --- Helpers ----------------------------------------------------------------
fetch_dataset <- function(code) {
  url <- glue("https://bdm.insee.fr/series/sdmx/data/{code}")
  message("→ Fetching ", code, " …")
  
  sdmx <- readSDMX(url)
  tbl <- as_tibble(sdmx)
  
  # Coerce common columns. We keep characters for IDs/dimensions; numeric for OBS_VALUE
  tbl <- tbl |>
    mutate(
      across(where(is.factor), as.character),
      OBS_VALUE = suppressWarnings(as.numeric(OBS_VALUE))
    )
  
  attr(tbl, "sdmx_url") <- url
  tbl
}

save_dataset <- function(code, x, out_dir = ".",
                         also_rdata = FALSE) {
  dir_create(out_dir)
  
  rds_path <- path(out_dir, glue("{code}.rds"))
  write_rds(x, rds_path, compress = "gz")
  
  if (also_rdata) {
    rdata_path <- path(out_dir, glue("{code}.RData"))
    # Save an object named exactly like the dataset code, for compatibility
    tmp_env <- rlang::env()
    rlang::env_bind(tmp_env, !!code := x)
    save(list = code, file = rdata_path, envir = tmp_env)
    message(glue("✔ Saved {code} → {rds_path} and {rdata_path}"))
  } else {
    message(glue("✔ Saved {code} → {rds_path}"))
  }
  
  invisible(rds_path)
}

# --- Download all -----------------------------------------------------------
results <- map(datasets, function(code) {
  # small retry loop
  last_err <- NULL
  for (i in 1:3) {
    try({
      tbl <- fetch_dataset(code)
      return(list(ok = TRUE, code = code, data = tbl))
    }, silent = TRUE)
    last_err <- geterrmessage()
    message("  ↻ Retry ", i, " failed for ", code, ": ", last_err)
  }
  list(ok = FALSE, code = code, error = last_err)
})

# --- Save & report ----------------------------------------------------------
walk(results, function(res) {
  if (isTRUE(res$ok)) {
    # Save .rds by default; set also_rdata = TRUE if you still need .RData files
    save_dataset(res$code, res$data, out_dir = ".", also_rdata = FALSE)
  } else {
    message("✖ ERROR for ", res$code, ": ", res$error,
            " — try _download_one_pieces.R")
  }
})

message("Done.")


