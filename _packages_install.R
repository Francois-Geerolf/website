#!/usr/bin/env Rscript

# Load package list
source("_packages.R")

format_time <- function(seconds) {
  minutes <- floor(seconds / 60)
  seconds <- round(seconds %% 60, 1)
  sprintf("%dm %.1fs", minutes, seconds)
}

# Ensure pak is available
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://cloud.r-project.org")
}

# Track already installed packages
already_installed <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)

cat("📦 Installing missing packages with pak...\n\n")

start_time <- proc.time()[3]

# Install all missing packages at once (key improvement)
to_install <- packages[!already_installed]

install_result <- tryCatch(
  {
    if (length(to_install) > 0) {
      pak::pkg_install(to_install)
    }
    TRUE
  },
  error = function(e) {
    cat("❌ Global installation error:\n", conditionMessage(e), "\n\n")
    FALSE
  }
)

total_elapsed <- proc.time()[3] - start_time

# Check final status package by package
cat("\n📋 Installation Summary:\n")

for (pkg in packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    if (already_installed[pkg]) {
      cat(sprintf(" - %s: ✅ already installed\n", pkg))
    } else {
      cat(sprintf(" - %s: ✅ installed\n", pkg))
    }
  } else {
    cat(sprintf(" - %s: ❌ failed\n", pkg))
  }
}

cat(sprintf("\n🕒 Total time: %s\n", format_time(total_elapsed)))
