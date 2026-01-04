# =====================================================================
# File: tools/x_factor_diffs.R
# Purpose: Compare original vs current code: package deltas, capability
#          capability probes, and HTML diff output
# =====================================================================

.DIFF_TOOL_VERSION <- "v2.6 (HTML always on, line-wise extractor, extra_current_paths, pretty pkg casing, renv.lock versions)"
if (!isTRUE(getOption("diff_tool.quiet"))) {
  message("x_factor_diffs: ", .DIFF_TOOL_VERSION)
}


# ----------------------------- Helpers --------------------------------

read_text <- function(path){
  if (grepl("^https?://", path)) {
    con <- url(path, "rb"); on.exit(close(con), add = TRUE)
    return(readLines(con, warn = FALSE, encoding = "UTF-8"))
  }
  if (!file.exists(path)) stop("Not found: ", path)
  readLines(path, warn = FALSE, encoding = "UTF-8")
}

write_md <- function(lines, path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path)
  message("✓ Wrote: ", normalizePath(path, FALSE))
}

extract_funcs <- function(lines) {
  rx <- "^\\s*([A-Za-z0-9_.]+)\\s*<-\\s*function\\s*\\(([^)]*)\\)"
  hits <- grep(rx, lines)
  if (!length(hits)) {
    return(data.frame(name = character(), args = character(), start = integer(), stringsAsFactors = FALSE))
  }
  do.call(
    rbind,
    lapply(hits, function(i) {
      m <- regexec(rx, lines[i])
      g <- regmatches(lines[i], m)[[1]]
      data.frame(name = g[2], args = trimws(g[3]), start = i, stringsAsFactors = FALSE)
    })
  )
}

loc_stats <- function(lines) {
  list(
    lines_total    = length(lines),
    lines_nonblank = sum(nzchar(trimws(lines))),
    lines_comments = sum(grepl("^\\s*#", lines))
  )
}

fn_stats <- function(lines, funs) {
  if (!nrow(funs)) return(list(count = 0, avg_len = 0, max_len = 0))
  starts <- funs$start
  ends   <- c(starts[-1] - 1L, length(lines))
  lens   <- pmax(0L, ends - starts + 1L)
  list(count = nrow(funs), avg_len = round(mean(lens), 1), max_len = max(lens))
}

extract_packages <- function(lines) {
  is_comment <- function(s) grepl("^\\s*#", s)
  has_call   <- function(s) grepl("\\b(library|require|requireNamespace)\\s*\\(", s)
  candidates <- lines[!is_comment(lines) & has_call(lines)]
  pkgs <- character(0)
  
  pull <- function(rx, s) {
    m  <- regexec(rx, s)
    mt <- regmatches(s, m)[[1]]
    if (length(mt)) tolower(mt[2]) else NA_character_
  }
  
  for (ln in candidates) {
    p <- pull("\\blibrary\\s*\\(\\s*['\"]?([A-Za-z0-9.]+)['\"]?\\s*\\)", ln)
    if (!is.na(p)) { pkgs <- c(pkgs, p); next }
    
    p <- pull("\\brequireNamespace\\s*\\(\\s*['\"]([A-Za-z0-9.]+)['\"]", ln)
    if (!is.na(p)) { pkgs <- c(pkgs, p); next }
    
    p <- pull("\\brequire\\s*\\(\\s*['\"]?([A-Za-z0-9.]+)['\"]?\\b", ln)
    if (!is.na(p)) { pkgs <- c(pkgs, p); next }
  }
  
  sort(unique(pkgs))
}

canon_pkg_name <- local({
  map <- c(
    "r.utils"   = "R.utils",
    "webshot2"  = "webshot2",
    "webshot"   = "webshot",
    "gt"        = "gt",
    "dplyr"     = "dplyr",
    "purrr"     = "purrr",
    "tidyr"     = "tidyr",
    "readxl"    = "readxl",
    "withr"     = "withr",
    "glue"      = "glue",
    "magick"    = "magick",
    "digest"    = "digest",
    "rlang"     = "rlang",
    "base64enc" = "base64enc",
    "chromote"  = "chromote",
    "htmltools" = "htmltools"
  )
  function(x) unname(ifelse(tolower(x) %in% names(map), map[tolower(x)], x))
})
pretty_pkg <- function(x) sort(unique(canon_pkg_name(x)))

read_lock_versions <- function(lock_path = "renv.lock") {
  if (!file.exists(lock_path)) return(NULL)
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    message("ℹ️ Installing jsonlite to read renv.lock …")
    install.packages("jsonlite", quiet = TRUE)
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)
  lock <- tryCatch(jsonlite::fromJSON(lock_path), error = function(e) NULL)
  if (is.null(lock) || is.null(lock$Packages)) return(NULL)
  vers <- vapply(lock$Packages, function(x) x$Version %||% NA_character_, character(1))
  names(vers) <- tolower(names(vers))
  vers
}
`%||%` <- function(a, b) if (!is.null(a)) a else b

has_any <- function(lines, patterns, ignore.case = TRUE) {
  any(vapply(
    patterns,
    function(p) any(grepl(p, lines, fixed = TRUE, ignore.case = ignore.case)),
    logical(1)
  ))
}

format_check <- function(ok, label) if (isTRUE(ok)) paste0("- [x] ", label) else paste0("- [ ] ", label)

package_diff <- function(pkgs_old, pkgs_new) {
  list(
    added   = setdiff(pkgs_new, pkgs_old),
    removed = setdiff(pkgs_old, pkgs_new),
    common  = intersect(pkgs_old, pkgs_new)
  )
}

ensure_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message("ℹ️ Installing ", pkg, " …")
    install.packages(pkg, quiet = TRUE)
  }
  requireNamespace(pkg, quietly = TRUE)
}

side_by_side_html <- function(orig_lines, curr_lines, out_html) {
  if (is.null(out_html)) return(invisible(FALSE))
  if (!ensure_pkg("diffobj")) {
    message("ℹ️ 'diffobj' not available; skipping HTML diff.")
    return(invisible(FALSE))
  }
  dir <- dirname(out_html)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  html <- diffobj::diffChr(orig_lines, curr_lines, format = "html", pager = "off")
  cat(as.character(html), file = out_html)
  message("✓ Wrote: ", normalizePath(out_html, FALSE))
  invisible(TRUE)
}

probe_features <- function(lines) {
  list(
    deterministic_session    = has_any(lines, c("withr::local_locale(", "withr::local_timezone(", "set.seed(")),
    renv_integration         = has_any(lines, c("renv::", "renv/activate.R")),
    baseline_hashing         = has_any(lines, c(
      "write_html_baseline(",
      "maybe_check_html_baseline(",
      "write_baseline_file(",
      "maybe_check_baseline_file("
    )),
    local_logos              = has_any(lines, c("assets/mlb", "get_logo_map(", "to_file_url(")),
    espn_logos               = has_any(lines, c("a.espncdn.com/i/teamlogos/mlb/500/")),
    google_font_quicksand    = has_any(lines, c("google_font(", "Quicksand")),
    base64_font_embed        = has_any(lines, c("base64enc", "embed_quicksand_or_fallback(", "dataURI(")),
    chromote_usage           = has_any(lines, c("requireNamespace(\"chromote\"", "find_chrome(")),
    webshot_usage            = has_any(lines, c("requireNamespace(\"webshot2\"", "webshot(", "phantomjs")),
    png_normalization        = has_any(lines, c("normalize_png(", "image_extent(", "image_resize(")),
    keyline                  = has_any(lines, c("add_inner_keyline(", "image_draw(", "rect(")),
    timeout_enforced         = has_any(lines, c("withTimeout(")),
    parameterized_league_div = has_any(lines, c("build_teams_table(", "league =", "division =")),
    hardcoded_division       = has_any(lines, c(
      'League == "NL"', 'League == "AL"', 'Division == "W"', 'Division == "E"',
      '== "NL" &', 'division_row == "W"'
    )),
    menu_runner              = has_any(lines, c("menu(", "file.choose(", "pick_sheet_for(", "Select league:", "Select division:"))
  )
}


# -------------------------- Core Comparison ---------------------------

narrative_compare <- function(original_path,
                              current_path,
                              label,
                              out_md,
                              out_html = NULL,
                              extra_current_paths = NULL) {
  orig <- read_text(original_path)
  curr <- read_text(current_path)
  
  curr_probe_lines <- curr
  if (length(extra_current_paths)) {
    for (p in extra_current_paths) {
      if (file.exists(p)) curr_probe_lines <- c(curr_probe_lines, read_text(p))
    }
  }
  
  st_o <- loc_stats(orig); st_c <- loc_stats(curr)
  fn_o <- fn_stats(orig, extract_funcs(orig)); fn_c <- fn_stats(curr, extract_funcs(curr))
  pk_o <- extract_packages(orig); pk_c <- extract_packages(curr)
  pd   <- package_diff(pk_o, pk_c)
  
  if (length(intersect(pd$added, pd$removed))) {
    stop("Package set contradiction (added ∩ removed non-empty) — extractor failed.", call. = FALSE)
  }
  
  feat_o <- probe_features(orig)
  feat_c <- probe_features(curr_probe_lines)
  
  pd_disp <- list(
    added   = pretty_pkg(pd$added),
    removed = pretty_pkg(pd$removed),
    common  = pretty_pkg(pd$common)
  )
  
  lock_versions <- read_lock_versions("renv.lock")  # may be NULL
  with_ver <- function(pkg_vec) {
    if (is.null(lock_versions) || !length(pkg_vec)) return(pkg_vec)
    vapply(pkg_vec, function(p) {
      v <- lock_versions[[tolower(p)]]
      if (is.null(v) || is.na(v) || !nzchar(v)) p else sprintf("%s (current v%s)", p, v)
    }, character(1))
  }
  
  pkg_added_lines   <- if (length(pd_disp$added))   paste0("  - ", with_ver(pd_disp$added))   else NULL
  pkg_removed_lines <- if (length(pd_disp$removed)) paste0("  - ", pd_disp$removed)            else NULL
  pkg_common_lines  <- if (length(pd_disp$common))  paste0("  - ", with_ver(pd_disp$common))  else NULL
  
  pkg_added   <- if (length(pkg_added_lines))   c("- Added:",     pkg_added_lines)   else "- Added: (none)"
  pkg_removed <- if (length(pkg_removed_lines)) c("- Removed:",   pkg_removed_lines) else "- Removed: (none)"
  pkg_common  <- if (length(pkg_common_lines))  c("- Unchanged:", pkg_common_lines)  else "- Unchanged: (none)"
  
  caps_new <- c(
    format_check(feat_c$parameterized_league_div && !feat_o$parameterized_league_div, "Parameterized league/division (no hard-coded divisions)"),
    format_check(feat_c$deterministic_session && !feat_o$deterministic_session,        "Deterministic session (locale, timezone, seed)"),
    format_check(feat_c$baseline_hashing && !feat_o$baseline_hashing,                  "Baseline hashing & drift enforcement"),
    format_check(feat_c$local_logos && !feat_o$local_logos,                            "Local logos (offline) with URL fallback"),
    format_check(feat_c$base64_font_embed && !feat_o$base64_font_embed,                "Local Quicksand font embedding (offline)"),
    format_check(feat_c$chromote_usage && !feat_o$chromote_usage,                      "Chromote backend support"),
    format_check(feat_c$webshot_usage && !feat_o$webshot_usage,                        "Webshot/Webshot2 support"),
    format_check(feat_c$png_normalization && !feat_o$png_normalization,                "PNG normalization to exact canvas"),
    format_check(feat_c$keyline && !feat_o$keyline,                                    "Inner keyline border"),
    format_check(feat_c$timeout_enforced && !feat_o$timeout_enforced,                  "Render timeout protection"),
    format_check(feat_c$menu_runner && !feat_o$menu_runner,                            "Menu-driven runner / sheet picker")
  )
  
  keep <- function(flag) isTRUE(flag)
  caps_retained <- c(
    if (keep(feat_c$parameterized_league_div && feat_o$parameterized_league_div)) "- Parameterized league/division",
    if (keep(feat_c$deterministic_session && feat_o$deterministic_session))       "- Deterministic session",
    if (keep(feat_c$baseline_hashing && feat_o$baseline_hashing))                 "- Baseline hashing & drift enforcement",
    if (keep(feat_c$local_logos && feat_o$local_logos))                           "- Local logos (offline) with URL fallback",
    if (keep(feat_c$base64_font_embed && feat_o$base64_font_embed))               "- Local Quicksand font embedding (offline)",
    if (keep(feat_c$chromote_usage && feat_o$chromote_usage))                     "- Chromote backend support",
    if (keep(feat_c$webshot_usage && feat_o$webshot_usage))                       "- Webshot/Webshot2 support",
    if (keep(feat_c$png_normalization && feat_o$png_normalization))               "- PNG normalization to exact canvas",
    if (keep(feat_c$keyline && feat_o$keyline))                                   "- Inner keyline border",
    if (keep(feat_c$timeout_enforced && feat_o$timeout_enforced))                 "- Render timeout protection",
    if (keep(feat_c$menu_runner && feat_o$menu_runner))                           "- Menu-driven runner / sheet picker"
  )
  if (!length(caps_retained)) caps_retained <- "- (none)"
  
  caps_removed <- c(
    if (keep(feat_o$parameterized_league_div && !feat_c$parameterized_league_div)) "- Parameterized league/division",
    if (keep(feat_o$deterministic_session && !feat_c$deterministic_session))       "- Deterministic session",
    if (keep(feat_o$baseline_hashing && !feat_c$baseline_hashing))                 "- Baseline hashing & drift enforcement",
    if (keep(feat_o$local_logos && !feat_c$local_logos))                           "- Local logos (offline) with URL fallback",
    if (keep(feat_o$base64_font_embed && !feat_c$base64_font_embed))               "- Local Quicksand font embedding (offline)",
    if (keep(feat_o$chromote_usage && !feat_c$chromote_usage))                     "- Chromote backend support",
    if (keep(feat_o$webshot_usage && !feat_c$webshot_usage))                       "- Webshot/Webshot2 support",
    if (keep(feat_o$png_normalization && !feat_c$png_normalization))               "- PNG normalization to exact canvas",
    if (keep(feat_o$keyline && !feat_c$keyline))                                   "- Inner keyline border",
    if (keep(feat_o$timeout_enforced && !feat_c$timeout_enforced))                 "- Render timeout protection",
    if (keep(feat_o$menu_runner && !feat_c$menu_runner))                           "- Menu-driven runner / sheet picker"
  )
  if (!length(caps_removed)) caps_removed <- "- (none)"
  
  md <- c(
    sprintf("# %s — Narrative Change Summary", tools::toTitleCase(label)),
    "",
    sprintf("- Original: `%s`", original_path),
    sprintf("- Current:  `%s`", current_path),
    "",
    "## Size & Structure",
    sprintf(
      "- Lines: %d → %d (nonblank: %d → %d; comments: %d → %d)",
      st_o$lines_total, st_c$lines_total, st_o$lines_nonblank, st_c$lines_nonblank, st_o$lines_comments, st_c$lines_comments
    ),
    sprintf(
      "- Functions: %d → %d (avg len: %s → %s, max len: %s → %s)",
      fn_o$count, fn_c$count, fn_o$avg_len, fn_c$avg_len, fn_o$max_len, fn_c$max_len
    ),
    "",
    "## Package Changes",
    pkg_added,
    pkg_removed,
    pkg_common,
    "",
    "## Capabilities — New",
    caps_new,
    "",
    "## Capabilities — Retained",
    caps_retained,
    "",
    "## Capabilities — Removed",
    caps_removed,
    "",
    "## Notable Hard-coding Removed / Safer Patterns",
    c(
      format_check(feat_o$hardcoded_division && !feat_c$hardcoded_division, "Removed hard-coded League/Division constants"),
      format_check(feat_o$espn_logos && feat_c$local_logos,                 "Replaced ESPN-only logos with local+fallback strategy")
    ),
    "",
    "## Rendering & Assets (Signals found)",
    paste0("- Original: ", paste(names(Filter(isTRUE, feat_o)), collapse = ", ")),
    paste0("- Current:  ", paste(names(Filter(isTRUE, feat_c)), collapse = ", ")),
    "",
    "## Notes",
    "- Feature presence is detected via fixed-string probes (no regex).",
    "- For exact line-by-line changes, open the side-by-side HTML diff or GitHub Compare."
  )
  
  write_md(md, out_md)
  side_by_side_html(orig, curr, out_html)
}


# ------------------------- Portfolio Runner ---------------------------

run_portfolio_diff_plus <- function(original_teams,
                                    original_hitters,
                                    original_pitchers,
                                    current_unified,
                                    out_dir = "build/diff",
                                    current_runner = "render_x_factor_update_graphics.R") {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  narrative_compare(
    original_path = original_teams,
    current_path  = current_unified,
    label         = "teams",
    out_md        = file.path(out_dir, "teams_changelog.md"),
    out_html      = file.path(out_dir, "teams_side_by_side.html"),
    extra_current_paths = c(current_runner)
  )
  
  narrative_compare(
    original_path = original_hitters,
    current_path  = current_unified,
    label         = "hitters",
    out_md        = file.path(out_dir, "hitters_changelog.md"),
    out_html      = file.path(out_dir, "hitters_side_by_side.html"),
    extra_current_paths = c(current_runner)
  )
  
  narrative_compare(
    original_path = original_pitchers,
    current_path  = current_unified,
    label         = "pitchers",
    out_md        = file.path(out_dir, "pitchers_changelog.md"),
    out_html      = file.path(out_dir, "pitchers_side_by_side.html"),
    extra_current_paths = c(current_runner)
  )
  
  idx <- c(
    "# Portfolio Diff — Index",
    "",
    "- [Teams changelog](teams_changelog.md) | [HTML diff](teams_side_by_side.html)",
    "- [Hitters changelog](hitters_changelog.md) | [HTML diff](hitters_side_by_side.html)",
    "- [Pitchers changelog](pitchers_changelog.md) | [HTML diff](pitchers_side_by_side.html)",
    "",
    "Open Markdown for narrative summaries; HTML for line-by-line diffs."
  )
  write_md(idx, file.path(out_dir, "INDEX.md"))
}


# ---------------------------- On Source -------------------------------

if (identical(environment(), globalenv()) && !exists(".x_factor_diffs", inherits = FALSE)) {
  .x_factor_diffs <- TRUE
  message("Loaded run_x_factor_diffs(). Call it with your original paths + current unified script.")
}
