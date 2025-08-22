#!/usr/bin/env Rscript

source("_packages.R")

format_time <- function(seconds) {
  minutes <- floor(seconds / 60)
  seconds <- round(seconds %% 60, 1)
  sprintf("%dm %.1fs", minutes, seconds)
}

times <- numeric(length(pkgs))
names(times) <- pkgs
total_time <- proc.time()[3]

for (i in seq_along(pkgs)) {
  pkg <- pkgs[i]
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("⏳ Installing", pkg, "\n")
    t <- tryCatch(
      system.time(install.packages(pkg, repos = "https://cloud.r-project.org", quiet = TRUE)),
      error = function(e) NA
    )
    if (!is.na(t[3]) && requireNamespace(pkg, quietly = TRUE)) {
      times[i] <- t[3]
      cat("✅", pkg, "installed in", format_time(t[3]), "\n\n")
    } else {
      times[i] <- NA
      cat("❌ Failed to install", pkg, "\n\n")
    }
  } else {
    times[i] <- 0
    cat("✅", pkg, "already installed\n\n")
  }
}

total_elapsed <- proc.time()[3] - total_time
cat("📋 Installation Summary:\n")
for (i in seq_along(times)) {
  status <- if (is.na(times[i])) "❌ failed" else if (times[i] == 0) "✅ already" else paste0("✅ ", format_time(times[i]))
  cat(sprintf(" - %s: %s\n", names(times)[i], status))
}
cat(sprintf("\n🕒 Total time: %s\n", format_time(total_elapsed)))
