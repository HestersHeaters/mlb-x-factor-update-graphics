# =====================================================================
# File: render_x_factor_update_graphics.R (repo root)
# Purpose: One-click runner for Teams / Hitters / Pitchers with
#          HTML baselines (per-sheet, stored in build/baselines/)
# =====================================================================


# ----------------- Load renv and Main Creation Script -----------------

if (file.exists("renv/activate.R")) source("renv/activate.R", local = TRUE)

main_candidates <- c(
  "create_x_factor_update_graphics.R",
  "R/create_x_factor_update_graphics.R"
)

loaded <- FALSE
for (cand in main_candidates) {
  if (file.exists(cand)) {
    source(cand, chdir = TRUE, local = TRUE)
    if (!exists("run_with_excel", mode = "function", inherits = TRUE)) {
      stop(sprintf("Loaded '%s' but could not find run_with_excel().", cand))
    }
    message(sprintf("✓ Loaded main script: %s", cand))
    loaded <- TRUE
    break
  }
}
if (!loaded) {
  stop(paste(
    "Could not find your main creation script.",
    "Expected one of:",
    paste(paste0("  - ", main_candidates), collapse = "\n"),
    sep = "\n"
  ))
}


# ------------------------------- Utils --------------------------------

ensure_dir <- function(p) if (!dir.exists(p)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

ask_yesno <- function(title) {
  i <- menu(c("Yes","No"), title = title)
  if (i == 0) stop("Cancelled.")
  i == 1
}

detect_png_backend <- function() {
  if (requireNamespace("chromote", quietly = TRUE)) {
    ch <- tryCatch(chromote::find_chrome(), error = function(e) NULL)
    if (!is.null(ch)) return("chromote")
  }
  if (requireNamespace("webshot2", quietly = TRUE) && nzchar(Sys.which("phantomjs"))) return("webshot2")
  if (requireNamespace("webshot2", quietly = TRUE)) return("webshot2(no phantomjs)")
  "none"
}

kind_noun <- function(kind) {
  k <- tolower(kind)
  if (k %in% c("teams","standings")) "Team"
  else if (k == "hitters") "Hitters"
  else if (k == "pitchers") "Pitchers"
  else tools::toTitleCase(k)
}

safe_component <- function(x) {
  x <- gsub("[/\\\\:]+", "-", x)
  x <- gsub("[[:cntrl:]]+", "", x)
  trimws(x)
}

`%||%` <- function(a,b) if (!is.null(a)) a else b

pick_sheet_for <- function(xlsx, title = "Select a sheet:") {
  sheets <- readxl::excel_sheets(xlsx)
  cat("Available sheets (exact names):\n")
  for (i in seq_along(sheets)) cat(sprintf("[%d] %s\n", i, sheets[i]))
  idx <- menu(sheets, title = title)
  if (idx < 1) stop("No sheet selected.")
  sheets[idx]
}


# ----------------------- Workbook Defaults (repo) ---------------------

default_workbook_for <- function(kind) {
  root <- "data"; k <- tolower(kind)
  if (k == "teams")    return(file.path(root, "team_data_x_factor_update.xlsx"))
  if (k == "hitters")  return(file.path(root, "hitter_data_x_factor_update.xlsx"))
  if (k == "pitchers") return(file.path(root, "pitcher_data_x_factor_update.xlsx"))
  NULL
}

resolve_workbook_for <- function(kind) {
  x <- default_workbook_for(kind)
  if (!is.null(x) && file.exists(x)) return(x)
  message(sprintf("Default workbook for '%s' not found at: %s", kind, x %||% "<NULL>"))
  message("Please choose the workbook manually …")
  p <- file.choose()
  if (!file.exists(p)) stop("Selected Excel file does not exist.")
  p
}


# ---------------- Naming Helpers (Slug Outputs/Folders) ---------------

to_snake <- function(x) {
  s <- tolower(as.character(x))
  s <- gsub("[^a-z0-9]+", "_", s)
  gsub("^_|_$", "", s)
}

sheet_slug <- function(x) {
  s <- tolower(x)
  s <- gsub("\\bend\\s+of\\s+season\\b\\s*(20\\d{2})", "eos_\\1", s)  # EOS YYYY -> eos_YYYY
  months <- tolower(month.name)
  for (i in seq_along(months)) {
    s <- gsub(paste0("\\b", months[i], "\\b\\s*(20\\d{2})"), paste0(months[i], "_\\1"), s)  # July 2025 -> july_2025
  }
  s <- gsub("[^a-z0-9]+", "_", s)
  gsub("^_|_$", "", s)
}

kind_slug <- function(kind) {
  k <- tolower(kind)
  if (k %in% c("teams","standings")) "teams"
  else if (k == "hitters") "hitters"
  else if (k == "pitchers") "pitchers"
  else to_snake(k)
}


# ----------------------------- Baselines ------------------------------

normalize_html_for_baseline <- function(path) {
  x <- readLines(path, warn = FALSE, encoding = "UTF-8")
  
  # Normalize line endings + trim trailing spaces
  x <- gsub("\r\n?", "\n", x)
  x <- sub("[ \t]+$", "", x, perl = TRUE)
  
  # gt gives a randomized outer <div id="xxxxxxxx"> and CSS with "#xxxxxxxx"
  # Replace any id="xxxx..." with a stable id, and rewrite selectors to match.
  # (ids are typically lower-case letters/numbers, length >= 8)
  x <- gsub('id="[a-z0-9]{8,}"', 'id="gt_stable"', x)
  x <- gsub('#[a-z0-9]{8,}(\\b)', '#gt_stable\\1', x)
  
  paste(x, collapse = "\n")
}

# Recompute baseline as SHA256 of the normalized HTML text
write_html_baseline <- function(rendered_html, baseline_path) {
  dir.create(dirname(baseline_path), recursive = TRUE, showWarnings = FALSE)
  norm_txt <- normalize_html_for_baseline(rendered_html)
  sha <- digest::digest(norm_txt, algo = "sha256")
  writeLines(sha, baseline_path)
  invisible(sha)
}

maybe_check_html_baseline <- function(rendered_html, baseline_path) {
  if (is.na(baseline_path)) return(invisible(TRUE))
  if (!file.exists(baseline_path)) {
    write_html_baseline(rendered_html, baseline_path)  # first run auto-init
    return(invisible(TRUE))
  }
  cur  <- digest::digest(normalize_html_for_baseline(rendered_html), algo = "sha256")
  base <- readLines(baseline_path, warn = FALSE)
  if (!identical(cur, base)) stop("❌ Drift detected vs baseline (normalized). If intentional, update baseline.")
  invisible(TRUE)
}
# ------------------------------- Menu ---------------------------------

kind_idx <- menu(c("Teams","Hitters","Pitchers","ALL"), title = "Select table type:")
if (kind_idx < 1) stop("No table type selected.")
kinds <- switch(kind_idx,
                c("teams"),
                c("hitters"),
                c("pitchers"),
                c("teams","hitters","pitchers")
)

scope <- menu(c("Single division","All six divisions"), title = "Render scope:")
if (scope < 1) stop("No scope selected.")

results <- list()


# ------------------------------ Execute -------------------------------

if (scope == 1) {
  lg <- c("AL","NL")[menu(c("AL","NL"), title = "Select league:")]
  if (is.na(lg)) stop("No league selected.")
  dv <- c("W","E","C")[menu(c("W","E","C"), title = "Select division:")]
  if (is.na(dv)) stop("No division selected.")
  tag <- paste0(lg, "_", dv)
  
  for (kind in kinds) {
    xlsx <- resolve_workbook_for(kind)
    sheet_choice <- pick_sheet_for(xlsx, sprintf("Select a sheet for %s:", kind))
    
    noun <- kind_noun(kind)
    ks   <- kind_slug(kind)
    
    folder <- file.path("outputs", ks, paste(ks, sheet_slug(sheet_choice), sep = "_"))
    ensure_dir(folder)
    
    file_stub_safe <- paste(to_snake(lg), to_snake(dv), ks, sheet_slug(sheet_choice), sep = "_")
    out_png  <- file.path(folder, paste0(file_stub_safe, ".png"))
    out_html <- file.path(folder, paste0(file_stub_safe, ".html"))
    
    bfile_read  <- baseline_read_path_for(lg, dv, kind, sheet_choice)
    bfile_write <- baseline_write_path_for(lg, dv, kind, sheet_choice)
    
    use_base <- file.exists(bfile_read) && ask_yesno(
      sprintf("Enforce baseline for %s [%s]? (%s)", kind, tag, bfile_read)
    )
    
    run_with_excel(
      input_xlsx  = xlsx,
      sheet       = sheet_choice,
      league      = lg,
      division    = dv,
      kind        = kind,
      output_png  = out_png,
      output_html = out_html,
      baseline    = if (use_base) bfile_read else NA_character_
    )
    
    wrote <- ask_yesno(sprintf("Write/Update baseline for %s [%s] now?", kind, tag))
    if (wrote) write_baseline_lower(out_html, bfile_write)
    
    results[[length(results)+1L]] <- data.frame(
      kind = kind, league = lg, division = dv,
      workbook = normalizePath(xlsx, FALSE),
      sheet = sheet_choice,
      png  = normalizePath(out_png,  FALSE),
      html = normalizePath(out_html, FALSE),
      baseline_read  = normalizePath(bfile_read,  FALSE),
      baseline_write = normalizePath(bfile_write, FALSE),
      baseline_read_exists = file.exists(bfile_read),
      enforced = use_base,
      wrote_baseline = wrote,
      stringsAsFactors = FALSE
    )
  }
  
} else {
  leagues   <- c("AL","NL")
  divisions <- c("W","E","C")
  
  kind_cfgs <- lapply(kinds, function(kind) {
    xlsx <- resolve_workbook_for(kind)
    sheet_choice <- pick_sheet_for(xlsx, sprintf("Select a sheet for %s:", kind))
    noun <- kind_noun(kind)
    ks   <- kind_slug(kind)
    folder <- file.path("outputs", ks, paste(ks, sheet_slug(sheet_choice), sep = "_"))
    ensure_dir(folder)
    list(kind=kind, xlsx=xlsx, sheet=sheet_choice, noun=noun, ks=ks, folder=folder)
  })
  
  enforce_all <- ask_yesno("Enforce existing baselines for ALL divisions (where present)?")
  write_all   <- ask_yesno("Write/Update baselines for ALL divisions after rendering?")
  
  for (cfg in kind_cfgs) {
    kind <- cfg$kind; xlsx <- cfg$xlsx; sheet_choice <- cfg$sheet; noun <- cfg$noun; ks <- cfg$ks; folder <- cfg$folder
    for (lg in leagues) for (dv in divisions) {
      tag <- paste0(lg, "_", dv)
      
      file_stub_safe <- paste(to_snake(lg), to_snake(dv), ks, sheet_slug(sheet_choice), sep = "_")
      out_png  <- file.path(folder, paste0(file_stub_safe, ".png"))
      out_html <- file.path(folder, paste0(file_stub_safe, ".html"))
      
      bfile_read  <- baseline_read_path_for(lg, dv, kind, sheet_choice)
      bfile_write <- baseline_write_path_for(lg, dv, kind, sheet_choice)
      use_base <- enforce_all && file.exists(bfile_read)
      
      status <- "ok"; note <- NA_character_
      res_try <- try({
        run_with_excel(
          input_xlsx  = xlsx,
          sheet       = sheet_choice,
          league      = lg,
          division    = dv,
          kind        = kind,
          output_png  = out_png,
          output_html = out_html,
          baseline    = if (use_base) bfile_read else NA_character_
        )
      }, silent = TRUE)
      
      if (inherits(res_try, "try-error")) {
        status <- "drift_or_error"; note <- as.character(res_try)
      } else if (write_all) {
        write_baseline_lower(out_html, bfile_write)
      }
      
      results[[length(results)+1L]] <- data.frame(
        kind = kind, league = lg, division = dv,
        workbook = normalizePath(xlsx, FALSE),
        sheet = sheet_choice,
        png  = normalizePath(out_png,  FALSE),
        html = normalizePath(out_html, FALSE),
        baseline_read  = normalizePath(bfile_read,  FALSE),
        baseline_write = normalizePath(bfile_write, FALSE),
        baseline_read_exists = file.exists(bfile_read),
        enforced = use_base,
        wrote_baseline = if (write_all) TRUE else NA,
        status = status,
        note = note,
        stringsAsFactors = FALSE
      )
    }
  }
}

if (length(results)) {
  res <- do.call(rbind, results); rownames(res) <- NULL; print(res)
}
cat("\n=== Done ===\nBackend:", detect_png_backend(), "\n")
