# =====================================================================
# File: preflight.R  (repo root)
# Purpose: Local environment checks with actionable fixes.
# =====================================================================

# --------------------- Banner & Status Helpers -------------------------

cat("== R Portfolio Preflight ==\n")
ok <- TRUE
fail <- function(msg) { cat("❌ ", msg, "\n", sep = ""); ok <<- FALSE }
pass <- function(msg) { cat("✅ ", msg, "\n", sep = "") }


# ---------------------- R version vs renv.lock -------------------------

want <- tryCatch(
  {
    if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite", quiet = TRUE)
    jsonlite::fromJSON("renv.lock")$R$Version
  },
  error = function(e) NA_character_
)

if (is.na(want)) {
  fail("Could not read R version from renv.lock.")
} else {
  have_mm <- paste0(R.version$major, ".", strsplit(R.version$minor, "[.]")[[1]][1])
  want_mm <- sub("^(\\d+\\.\\d+).*", "\\1", want)
  if (have_mm == want_mm) pass(sprintf("R version OK: have %s, lockfile %s", have_mm, want))
  else fail(sprintf("R version mismatch: have %s, lockfile %s. Install R %s.x.", have_mm, want, want_mm))
}


# -------------------- Renv Presence & Activation ----------------------

renv_present <- requireNamespace("renv", quietly = TRUE) && file.exists("renv/activate.R")
if (!renv_present) {
  fail("renv not ready. Run: install.packages('renv'); then source('bootstrap.R').")
} else {
  act <- any(grepl("renv/library", normalizePath(.libPaths(), winslash = "/"), fixed = TRUE))
  if (act) pass("renv active.") else fail("renv not active. Restart session or source('.Rprofile').")
}


# ----------------------------- macOS CLT ------------------------------

is_macos <- identical(tolower(Sys.info()[["sysname"]]), "darwin")
if (is_macos) {
  has_clt <- tryCatch(
    {
      p <- suppressWarnings(system("xcode-select -p", intern = TRUE))
      is.character(p) && length(p) >= 1 && nzchar(p[1])
    },
    error = function(e) FALSE
  )
  if (has_clt) pass("macOS Command Line Tools detected.")
  else fail("macOS CLT missing. In Terminal: xcode-select --install (then re-run preflight).")
} else {
  pass("Non-macOS system—CLT not required.")
}


# ---------------------------- PNG Backend -----------------------------

backend_ok <- FALSE
if (requireNamespace("chromote", quietly = TRUE)) {
  pass("PNG backend: chromote installed.")
  backend_ok <- TRUE
} else if (requireNamespace("webshot2", quietly = TRUE)) {
  ph <- nzchar(Sys.which("phantomjs"))
  if (ph) { pass("PNG backend: webshot2 + PhantomJS found.") ; backend_ok <- TRUE }
  else fail("webshot2 installed but PhantomJS missing. In R: webshot2::install_phantomjs()")
} else {
  fail("No PNG backend. Install in R: install.packages('chromote') or 'webshot2'")
}


# ----------------------- Required Excel Inputs ------------------------

inputs <- c(
  "data/team_data_x_factor_update.xlsx",
  "data/hitter_data_x_factor_update.xlsx",
  "data/pitcher_data_x_factor_update.xlsx"
)
missing <- inputs[!file.exists(inputs)]

if (length(missing) == 0) {
  pass("Excel inputs found in data/.")
} else {
  fail(paste0(
    "Missing Excel files:\n  - ",
    paste(basename(missing), collapse = "\n  - "),
    "\nPlace them under data/ with these exact names."
  ))
}


# --------------------- Assets / Fonts Presence ------------------------

if (dir.exists("assets/mlb")) pass("Team logos present (assets/mlb).") else fail("assets/mlb/ missing (will fallback to web logos).")
if (dir.exists("assets/quicksand_font")) pass("Quicksand font folder present.") else fail("assets/quicksand_fonts/ missing (Google Fonts fallback).")
if (requireNamespace("base64enc", quietly = TRUE)) pass("base64enc present (offline font embedding).") else cat("ℹ️ base64enc not installed (optional; fonts will use Google Fonts).\n")


# ----------------------------- Summary --------------------------------

if (ok) {
  cat("\n🎉 Preflight passed.\nNext:\n  source('render_x_factor_update_graphics.R')\n")
} else {
  cat("\n⚠️  Preflight failed. Fix the ❌ items above, then re-run:\n  source('preflight.R')\n")
}
