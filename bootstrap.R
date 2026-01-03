# =====================================================================
# File: bootstrap.R  (repo root)
# Purpose: Stable restore across Macs (fix 'package not available' / later)
# =====================================================================

# ------------------------- Start-Up Message ---------------------------

message("▶ Bootstrapping project …")


# ----------- Reliable CRAN Mirror & Robust Download Method ------------

options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))
options(download.file.method = "libcurl")


# -------------------------- Source Fallback ---------------------------

sys <- tolower(Sys.info()[["sysname"]])
if (identical(sys, "darwin")) {
  options(pkgType = "both")  # try binary, then source
  Sys.setenv(CURL_SSL_BACKEND = "secure-transport")
}


# ------------------------- Activate Project ---------------------------

if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
} else {
  if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")
  renv::activate(".")
}


# ------------------- Restore or Rebuild if Needed ---------------------

message("▶ renv::restore(rebuild = TRUE) …")
ok <- tryCatch(
  {
    renv::restore(prompt = FALSE, rebuild = TRUE)
    TRUE
  },
  error = function(e) {
    message("❌ restore failed: ", conditionMessage(e))
    FALSE
  }
)

if (!ok) {
  pkgs <- c("later", "curl")
  message("▶ Targeted retry (source) for: ", paste(pkgs, collapse = ", "))
  try(renv::install(pkgs, rebuild = TRUE), silent = TRUE)
  ok <- tryCatch(
    {
      renv::restore(prompt = FALSE, rebuild = TRUE)
      TRUE
    },
    error = function(e) FALSE
  )
}
if (!ok) stop("Aborting: renv::restore() failed after retry.")


# ----------------------------- PNG Backend ----------------------------

if (!requireNamespace("webshot2", quietly = TRUE)) renv::install("webshot2")
if (!requireNamespace("chromote", quietly = TRUE)) renv::install("chromote")
has_chrome <- tryCatch(!is.null(chromote::find_chrome()), error = function(e) FALSE)
if (!has_chrome) message("ℹ️ Chrome/Chromium not detected. Install Google Chrome for PNG export.")


# --------------------------- macOS CLT Note ---------------------------

if (identical(sys, "darwin")) {
  has_clt <- tryCatch(
    {
      p <- suppressWarnings(system("xcode-select -p", intern = TRUE))
      is.character(p) && length(p) >= 1 && nzchar(p[1])
    },
    error = function(e) FALSE
  )
  if (!has_clt) message("ℹ️ If source builds fail, run in Terminal: xcode-select --install")
}


# ----------------------------- Preflight ------------------------------

if (file.exists("preflight.R")) {
  message("▶ Running preflight …")
  try(source("preflight.R"), silent = TRUE)
}

message("✅ Bootstrap complete. Next:  source('render_x_factor_update_graphics.R')")