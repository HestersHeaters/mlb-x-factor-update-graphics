# =====================================================================
# File: .Rprofile  (repo root)
# Purpose: Project-local R session defaults; safe, non-invasive.
# =====================================================================

options(
  repos = c(CRAN = "https://packagemanager.posit.co/cran/2024-12-01"),
  save.workspace = FALSE
)

try({
  if (file.exists("renv/activate.R")) source("renv/activate.R", local = TRUE)
}, silent = TRUE)

invisible(TRUE)
