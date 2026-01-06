# =====================================================================
# File: make_diffs.R  (repo root)
# Purpose: One-click generation of narrative changelogs and HTML
#          side-by-side diffs
# =====================================================================

# ------------------------ Prepare Output Dir --------------------------

dir.create("build/diff", recursive = TRUE, showWarnings = FALSE)


# -------------------------- Load Diff Tool ----------------------------

source("tools/x_factor_diffs.R")


# ----------------- Define Original Script Locations -------------------

branch <- "original---5.10.25"

orig_local <- list(
  teams    = "original/X Factor Update Team Code - 5.10.25.R",
  hitters  = "original/X Factor Update Hitter Code - 5.10.25.R",
  pitchers = "original/X Factor Update Pitcher Code - 5.10.25.R"
)

orig_remote <- function(name) {
  paste0(
    "https://raw.githubusercontent.com/HestersHeaters/mlb-x-factor-update-graphics/",
    branch, "/original/", utils::URLencode(name, reserved = TRUE)
  )
}

pick_path <- function(local_path, remote_name) {
  if (file.exists(local_path)) local_path else orig_remote(remote_name)
}

orig_teams    <- pick_path(orig_local$teams,    basename(orig_local$teams))
orig_hitters  <- pick_path(orig_local$hitters,  basename(orig_local$hitters))
orig_pitchers <- pick_path(orig_local$pitchers, basename(orig_local$pitchers))


# ------------------ Resolve Current Unified Script --------------------

unified_candidates <- c(
  "create_x_factor_update_graphics.R",
  "R/create_x_factor_update_graphics.R"
)
current_unified <- NULL
for (cand in unified_candidates) {
  if (file.exists(cand)) { current_unified <- cand; break }
}
if (is.null(current_unified)) {
  stop(paste(
    "Missing unified script. Tried:",
    paste(paste0("  - ", unified_candidates), collapse = "\n"),
    sep = "\n"
  ), call. = FALSE)
}


# ------------------------ Validate Originals --------------------------

is_url <- function(p) grepl("^https?://", p, ignore.case = TRUE)

check_path_ok <- function(p) {
  if (is_url(p)) {
    ok <- try({
      con <- url(p, "rb"); on.exit(close(con), add = TRUE)
      readLines(con, n = 1, warn = FALSE)
      TRUE
    }, silent = TRUE)
    isTRUE(ok)
  } else {
    file.exists(p)
  }
}

named <- c(
  orig_teams    = orig_teams,
  orig_hitters  = orig_hitters,
  orig_pitchers = orig_pitchers
)

ok_vec <- vapply(named, check_path_ok, logical(1))
if (!all(ok_vec)) {
  bad <- names(ok_vec)[!ok_vec]
  bad_vals <- unname(named[bad])
  stop(paste(
    "Missing original file(s) or unreachable URL(s):",
    paste(paste0("  - ", bad_vals), collapse = "\n"),
    "• If using URLs, ensure they are raw.githubusercontent.com links.",
    "• Or place the originals under original/ with the exact filenames.",
    sep = "\n"
  ), call. = FALSE)
}


# -------------------------- Generate Diffs ----------------------------

run_portfolio_diff_plus(
  original_teams    = orig_teams,
  original_hitters  = orig_hitters,
  original_pitchers = orig_pitchers,
  current_unified   = current_unified,
  out_dir           = "build/diff"
)

message("\n✅ Diffs ready. Open build/diff/INDEX.md for links.\n")
