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

base <- "https://raw.githubusercontent.com/HestersHeaters/mlb-x-factor-update-graphics/original---5.10.25"
orig_teams    <- paste0(base, "/original/X Factor Update Team Code - 5.10.25.R")
orig_hitters  <- paste0(base, "/original/X Factor Update Hitter Code - 5.10.25.R")
orig_pitchers <- paste0(base, "/original/X Factor Update Pitcher Code - 5.10.25.R")


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

missing <- c(
  orig_teams    = orig_teams[!file.exists(orig_teams)],
  orig_hitters  = orig_hitters[!file.exists(orig_hitters)],
  orig_pitchers = orig_pitchers[!file.exists(orig_pitchers)]
)
if (length(missing)) {
  stop(paste(
    "Missing original file(s):",
    paste(paste0("  - ", unname(missing)), collapse = "\n"),
    "Fix the paths above or place the files under original/ with exact names.",
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
