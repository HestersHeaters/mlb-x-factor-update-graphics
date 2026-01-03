# =====================================================================
# File: create_x_factor_update_graphics.R (repo root)
# Purpose: Engines for Teams / Hitters / Pitchers; offline-stable:
#          local quicksand font + local logos; identical visuals.
# =====================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(gt)
  library(readxl)
  library(glue)
  library(withr)
  library(R.utils)
  library(magick)
  library(rlang)
  library(digest)   
  library(base64enc)
})


# ---------------------------- Determinism -----------------------------

deterministic_session <- function() {
  withr::local_locale(c(
    "LC_TIME"    = "en_US.UTF-8",
    "LC_COLLATE" = "en_US.UTF-8",
    "LC_NUMERIC" = "C"
  ))
  withr::local_timezone("America/New_York")
  set.seed(12345)
  options(stringsAsFactors = FALSE, scipen = 999)
}


# ----------------------------- Backend --------------------------------

ensure_png_backend <- function() {
  if (requireNamespace("webshot2", quietly = TRUE) &&
      requireNamespace("chromote", quietly = TRUE) &&
      isTRUE(tryCatch(!is.null(chromote::find_chrome()), error = function(e) FALSE))) {
    return(invisible("webshot2+chromote"))
  }
  if (requireNamespace("webshot", quietly = TRUE) && nzchar(Sys.which("phantomjs"))) {
    return(invisible("webshot+phantomjs"))
  }
  stop(paste(
    "PNG export needs either:",
    "  • webshot2 + chromote + Chrome/Chromium (recommended), or",
    "  • webshot + PhantomJS (legacy).",
    "Fix: renv::install(c('webshot2','chromote')); then install Google Chrome.",
    sep = "\n"
  ))
}

detect_png_backend <- function() {
  if (requireNamespace("webshot2", quietly = TRUE) &&
      requireNamespace("chromote", quietly = TRUE) &&
      isTRUE(tryCatch(!is.null(chromote::find_chrome()), error = function(e) FALSE))) {
    "webshot2+chromote"
  } else if (requireNamespace("webshot", quietly = TRUE) && nzchar(Sys.which("phantomjs"))) {
    "webshot+phantomjs"
  } else {
    "none"
  }
}


# ------------------------- File System Utils -------------------------

ensure_parent_dir <- function(path) {
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}


# ---------------------- Baseline (HTML Hash) -------------------------

compute_sha256 <- function(path) digest::digest(file = path, algo = "sha256")

write_html_baseline <- function(rendered_html, baseline_path) {
  dir.create(dirname(baseline_path), recursive = TRUE, showWarnings = FALSE)
  sha <- compute_sha256(rendered_html)
  writeLines(sha, baseline_path)
  invisible(sha)
}

maybe_check_html_baseline <- function(rendered_html, baseline_path) {
  if (is.na(baseline_path)) return(invisible(TRUE))
  if (!file.exists(baseline_path)) {
    write_html_baseline(rendered_html, baseline_path)  # first run auto-init
    return(invisible(TRUE))
  }
  cur  <- compute_sha256(rendered_html)
  base <- readLines(baseline_path, warn = FALSE)
  if (!identical(cur, base)) stop("❌ Drift detected vs baseline. Re-run write_html_baseline() if intentional.")
  invisible(TRUE)
}


# ---------------------- Baseline (PNG Hash) --------------------------

compute_sha256_file <- function(path) digest::digest(file = path, algo = "sha256")

write_baseline_file <- function(rendered_png, baseline_path) {
  dir.create(dirname(baseline_path), recursive = TRUE, showWarnings = FALSE)
  sha <- compute_sha256_file(rendered_png)
  writeLines(sha, baseline_path)
  invisible(sha)
}

maybe_check_baseline_file <- function(rendered_png, baseline_path) {
  if (is.na(baseline_path)) return(invisible(TRUE))
  if (!file.exists(baseline_path)) {
    # First run: auto-initialize to current PNG
    write_baseline_file(rendered_png, baseline_path)
    return(invisible(TRUE))
  }
  cur  <- compute_sha256_file(rendered_png)
  base <- readLines(baseline_path, warn = FALSE)
  if (!identical(cur, base)) stop("❌ Drift detected vs baseline. Re-run write_baseline_file() if intentional.")
  invisible(TRUE)
}

# ---------------------- Data-URI for Local Images --------------------

to_data_uri <- function(path) {
  stopifnot(file.exists(path))
  if (!requireNamespace("base64enc", quietly = TRUE)) stop("Install 'base64enc' to embed logos.")
  mime <- "image/png"
  base64enc::dataURI(file = path, mime = mime)
}
is_http_url <- function(x) is.character(x) && grepl("^https?://", x, ignore.case = TRUE)


# ----------------------- Image Post-Processing -----------------------

normalize_png <- function(in_path,
                          out_path,
                          target_w,
                          target_h,
                          gravity = "north",
                          color = "white",
                          trim_white = TRUE,
                          trim_fuzz = "3%",
                          preserve_edges = TRUE) {
  img <- image_read(in_path)
  img <- image_background(img, color = color, flatten = TRUE)
  if (trim_white) img <- image_trim(img, fuzz = trim_fuzz)
  info <- image_info(img)[1, ]
  w <- info$width
  h <- info$height
  if (any(is.na(c(w, h))) || w <= 0 || h <= 0) stop("Bad image size.")
  s <- min(target_w / w, target_h / h)
  new_w <- max(1L, as.integer(round(w * s)))
  new_h <- max(1L, as.integer(round(h * s)))
  img_out <- image_resize(
    img,
    paste0(new_w, "x", new_h, "!"),
    filter = if (preserve_edges) "point" else NULL
  ) |>
    image_extent(
      paste0(target_w, "x", target_h),
      gravity = gravity,
      color = color
    ) |>
    image_strip()
  image_write(img_out, out_path)
}

add_inner_keyline <- function(path, color = "black", lwd = 1) {
  img <- image_read(path)
  w <- image_info(img)$width
  h <- image_info(img)$height
  d <- image_draw(img)
  rect(0, 0, w - 1, h - 1, border = color, lwd = lwd)
  dev.off()
  d <- image_strip(d)
  image_write(d, path)
}


# ------------------- Robust Local Quicksand Embed --------------------

locate_quicksand_local <- function() {
  cands <- c(
    # canonical (no /static)
    file.path("assets", "quicksand_font", "Quicksand-Regular.woff2"),
    file.path("assets", "quicksand_font", "Quicksand-Bold.woff2"),
    file.path("assets", "quicksand_font", "Quicksand-Regular.ttf"),
    file.path("assets", "quicksand_font", "Quicksand-Bold.ttf"),
    file.path("assets", "quicksand_font", "Quicksand-VariableFont_wght.ttf"),
    # legacy with /static
    file.path("assets", "quicksand_font", "static", "Quicksand-Regular.woff2"),
    file.path("assets", "quicksand_font", "static", "Quicksand-Bold.woff2"),
    file.path("assets", "quicksand_font", "static", "Quicksand-Regular.ttf"),
    file.path("assets", "quicksand_font", "static", "Quicksand-Bold.ttf"),
    # older roots
    file.path("quicksand_font", "static", "Quicksand-Regular.ttf"),
    file.path("quicksand_font", "static", "Quicksand-Bold.ttf"),
    file.path("quicksand_font", "Quicksand-VariableFont_wght.ttf")
  )
  
  existing <- cands[file.exists(cands)]
  if (!length(existing)) return(NULL)
  
  pick <- function(patterns) {
    for (p in patterns) {
      hit <- existing[grepl(p, basename(existing), ignore.case = TRUE)]
      if (length(hit)) return(hit[1L])
    }
    NULL
  }
  
  reg <- pick(c("Regular\\.woff2$", "Regular\\.ttf$", "VariableFont_wght\\.ttf$"))
  bld <- pick(c("Bold\\.woff2$",    "Bold\\.ttf$",    "VariableFont_wght\\.ttf$"))
  
  to_mime <- function(p) {
    ext <- tolower(tools::file_ext(p))
    if (ext == "woff2") "font/woff2" else "font/ttf"
  }
  
  list(
    regular = if (!is.null(reg)) list(path = reg, mime = to_mime(reg)) else NULL,
    bold    = if (!is.null(bld)) list(path = bld, mime = to_mime(bld)) else NULL
  )
}

embed_quicksand_or_fallback <- function(tbl) {
  lf <- locate_quicksand_local()
  has_b64 <- requireNamespace("base64enc", quietly = TRUE)
  
  if (!is.null(lf) && has_b64 && (!is.null(lf$regular) || !is.null(lf$bold))) {
    to_data <- function(x) base64enc::dataURI(file = x$path, mime = x$mime)
    reg_uri <- if (!is.null(lf$regular)) to_data(lf$regular) else NULL
    bld_uri <- if (!is.null(lf$bold)) to_data(lf$bold) else NULL
    
    parts <- c()
    if (!is.null(reg_uri)) {
      parts <- c(parts, sprintf(
        "@font-face{font-family:'QuicksandLocal';src:url('%s') format('%s');font-weight:400;font-style:normal;font-display:swap;}",
        reg_uri,
        if (grepl("woff2", reg_uri)) "woff2" else "truetype"
      ))
    }
    if (!is.null(bld_uri)) {
      parts <- c(parts, sprintf(
        "@font-face{font-family:'QuicksandLocal';src:url('%s') format('%s');font-weight:700;font-style:normal;font-display:swap;}",
        bld_uri,
        if (grepl("woff2", bld_uri)) "woff2" else "truetype"
      ))
    }
    parts <- c(
      parts,
      "html, body{font-family:'QuicksandLocal', sans-serif;}",
      ".gt_table, .gt_heading, .gt_title, .gt_subtitle, .gt_col_headings, .gt_sourcenotes, .gt_from_md{font-family:'QuicksandLocal', sans-serif;}",
      ".gt_table *, .gt_heading *, .gt_col_headings *, .gt_sourcenotes *{font-family:'QuicksandLocal', sans-serif;}",
      ".gt_table, .gt_table *, .gt_heading, .gt_heading *, .gt_col_headings, .gt_col_headings *, .gt_sourcenotes, .gt_sourcenotes *{font-weight:700;}"
    )
    css <- paste(parts, collapse = "\n")
    
    if (utils::packageVersion("gt") >= "0.9.0") {
      return(gt::opt_css(tbl, css))
    } else {
      return(gt::tab_options(tbl, table.additional_css = css))
    }
  }
  
  gt::opt_table_font(tbl, font = list(gt::google_font("Quicksand"), gt::default_fonts()), weight = "bold")
}

verify_local_font <- function() {
  lf <- locate_quicksand_local()
  has_b64 <- requireNamespace("base64enc", quietly = TRUE)
  if (is.null(lf)) {
    message("⚠️ Font: no local Quicksand found → using google_font('Quicksand').")
    return(invisible(FALSE))
  }
  reg <- if (!is.null(lf$regular)) basename(lf$regular$path) else "-"
  bld <- if (!is.null(lf$bold)) basename(lf$bold$path) else "-"
  if (!has_b64) {
    message(sprintf(
      "⚠️ Font: local files found (Regular: %s, Bold: %s) but 'base64enc' not installed → falling back to google_font.",
      reg,
      bld
    ))
    return(invisible(FALSE))
  }
  message(sprintf("✅ Font: embedding local Quicksand (Regular: %s, Bold: %s).", reg, bld))
  invisible(TRUE)
}


# --------------------------- Logos Mapping ---------------------------

get_logo_map <- function() {
  local_dir <- "assets/mlb"
  abbrs <- c(
    "ARI", "ATL", "BAL", "BOS", "CHC", "CHW", "CIN", "CLE", "COL", "DET",
    "HOU", "KCR", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "ATH",
    "PHI", "PIT", "SDP", "SEA", "SFG", "STL", "TBR", "TEX", "TOR", "WSN"
  )
  local_paths <- setNames(file.path(local_dir, paste0(tolower(abbrs), ".png")), abbrs)
  if (all(file.exists(local_paths))) return(local_paths)
  c(
    ARI = "https://a.espncdn.com/i/teamlogos/mlb/500/ari.png",
    ATL = "https://a.espncdn.com/i/teamlogos/mlb/500/atl.png",
    BAL = "https://a.espncdn.com/i/teamlogos/mlb/500/bal.png",
    BOS = "https://a.espncdn.com/i/teamlogos/mlb/500/bos.png",
    CHC = "https://a.espncdn.com/i/teamlogos/mlb/500/chc.png",
    CHW = "https://a.espncdn.com/i/teamlogos/mlb/500/chw.png",
    CIN = "https://a.espncdn.com/i/teamlogos/mlb/500/cin.png",
    CLE = "https://a.espncdn.com/i/teamlogos/mlb/500/cle.png",
    COL = "https://a.espncdn.com/i/teamlogos/mlb/500/col.png",
    DET = "https://a.espncdn.com/i/teamlogos/mlb/500/det.png",
    HOU = "https://a.espncdn.com/i/teamlogos/mlb/500/hou.png",
    KCR = "https://a.espncdn.com/i/teamlogos/mlb/500/kc.png",
    LAA = "https://a.espncdn.com/i/teamlogos/mlb/500/laa.png",
    LAD = "https://a.espncdn.com/i/teamlogos/mlb/500/lad.png",
    MIA = "https://a.espncdn.com/i/teamlogos/mlb/500/mia.png",
    MIL = "https://a.espncdn.com/i/teamlogos/mlb/500/mil.png",
    MIN = "https://a.espncdn.com/i/teamlogos/mlb/500/min.png",
    NYM = "https://a.espncdn.com/i/teamlogos/mlb/500/nym.png",
    NYY = "https://a.espncdn.com/i/teamlogos/mlb/500/nyy.png",
    ATH = "https://a.espncdn.com/i/teamlogos/mlb/500/oak.png",
    PHI = "https://a.espncdn.com/i/teamlogos/mlb/500/phi.png",
    PIT = "https://a.espncdn.com/i/teamlogos/mlb/500/pit.png",
    SDP = "https://a.espncdn.com/i/teamlogos/mlb/500/sd.png",
    SEA = "https://a.espncdn.com/i/teamlogos/mlb/500/sea.png",
    SFG = "https://a.espncdn.com/i/teamlogos/mlb/500/sf.png",
    STL = "https://a.espncdn.com/i/teamlogos/mlb/500/stl.png",
    TBR = "https://a.espncdn.com/i/teamlogos/mlb/500/tb.png",
    TEX = "https://a.espncdn.com/i/teamlogos/mlb/500/tex.png",
    TOR = "https://a.espncdn.com/i/teamlogos/mlb/500/tor.png",
    WSN = "https://a.espncdn.com/i/teamlogos/mlb/500/wsh.png"
  )
}

is_http_url <- function(x) is.character(x) && grepl("^https?://", x, ignore.case = TRUE)
to_file_url <- function(path) paste0("file://", normalizePath(path, winslash = "/", mustWork = FALSE))


# ============================ TEAMS BUILDER ===========================

compute_team_order <- function(filtered_df) {
  stand <- filtered_df %>%
    dplyr::filter(Team == "Standing") %>%
    dplyr::select(-Team)
  ord_to_num <- function(x) suppressWarnings(as.numeric(sub("(st|nd|rd|th)$", "", as.character(x))))
  ranks <- vapply(stand[1, , drop = TRUE], ord_to_num, numeric(1))
  colnames(stand)[order(ranks)]
}

build_teams_table <- function(df_input, league = "NL", division = "W") {
  league <- toupper(league)
  division <- toupper(division)
  df <- df_input
  league_row <- df[1, -1]
  division_row <- df[2, -1]
  team_names <- names(df)[-1]
  division_cols <- which(league_row == league & division_row == division)
  if (!length(division_cols)) stop(glue("No {league} {division} team columns found."))
  filtered_df <- df[-c(1, 2), c(1, division_cols + 1)]
  colnames(filtered_df) <- c("Team", team_names[division_cols])
  keep_stats <- c("Standing", "Current Record", "Proj Record", "Wins vs. Proj", "CP %", "Proj Playoff %")
  filtered_df <- filtered_df %>% dplyr::filter(Team %in% keep_stats)
  row_index <- which(filtered_df$Team == "Wins vs. Proj")
  if (length(row_index) == 1) {
    numeric_vals <- round(as.numeric(unlist(filtered_df[row_index, -1])))
    formatted_vals <- ifelse(numeric_vals > 0, paste0("+", numeric_vals), as.character(numeric_vals))
    filtered_df[row_index, -1] <- as.list(formatted_vals)
  }
  cp_vals <- as.numeric(unlist(df[df[[1]] == "CP %", division_cols + 1]))
  proj_vals <- as.numeric(unlist(df[df[[1]] == "Proj Playoff %", division_cols + 1]))
  playoff_diff <- round((cp_vals - proj_vals) * 100, 1)
  playoff_diff_fmt <- ifelse(playoff_diff > 0, paste0("+", playoff_diff, "%"), paste0(playoff_diff, "%"))
  playoff_diff_row <- c("Playoff % Diff", playoff_diff_fmt)
  filtered_df <- dplyr::bind_rows(filtered_df, setNames(as.list(playoff_diff_row), names(filtered_df)))
  for (row_label in c("CP %", "Proj Playoff %")) {
    idx <- which(filtered_df$Team == row_label)
    if (length(idx) == 1) {
      vals <- as.numeric(unlist(filtered_df[idx, -1])) * 100
      formatted <- paste0(round(vals, 1), "%")
      filtered_df[idx, -1] <- as.list(formatted)
    }
  }
  row_order <- c("Standing", "Current Record", "Proj Record", "Wins vs. Proj", "CP %", "Proj Playoff %", "Playoff % Diff")
  filtered_df <- filtered_df %>%
    dplyr::mutate(row_factor = factor(Team, levels = row_order)) %>%
    dplyr::arrange(row_factor) %>%
    dplyr::select(-row_factor)
  team_order <- compute_team_order(filtered_df)
  filtered_df <- filtered_df[, c("Team", team_order)]
  
  logos <- get_logo_map()
  
  logo_src <- vapply(
    names(logos),
    function(k) {
      p <- logos[[k]]
      if (!is_http_url(p) && file.exists(p)) to_data_uri(p) else p
    },
    FUN.VALUE = character(1)
  )
  
  header_with_logos <- purrr::map(
    team_order,
    ~gt::html(paste0(
      '<div style="text-align:center; font-weight:bold; font-size:20px;">',
      '<img src="', logo_src[.x], '" height=50><br><span>', .x, '</span></div>'
    ))
  ) %>%
    rlang::set_names(team_order)
  
  gt_table <- gt::gt(
    filtered_df %>%
      dplyr::mutate(Team = dplyr::recode(
        Team,
        "Proj Record"    = "Projected Record",
        "Wins vs. Proj"  = "Wins vs. Projection",
        "CP %"           = "Current Playoff %",
        "Proj Playoff %" = "Projected Playoff %",
        "Playoff % Diff" = "Playoff % vs. Projection"
      ))
  ) %>%
    gt::tab_header(
      title = gt::md("<div style='font-size:60px; font-weight:bold; color:white;'>Current Team Standings</div>")
    ) %>%
    gt::tab_style(
      style = gt::cell_fill(color = "black"),
      locations = gt::cells_title(groups = "title")
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_text(align = "center", weight = "bold", size = gt::px(20))
      ),
      locations = gt::cells_body(columns = gt::everything())
    ) %>%
    gt::tab_style(
      style = gt::cell_text(align = "center", weight = "bold", size = gt::px(20)),
      locations = gt::cells_column_labels(columns = gt::everything())
    ) %>%
    gt::cols_width(
      Team ~ gt::px(180),
      gt::everything() ~ gt::px(120)
    ) %>%
    gt::cols_label(.list = header_with_logos) %>%
    gt::tab_options(
      data_row.padding = gt::px(25),
      heading.padding = gt::px(10),
      column_labels.padding = gt::px(4),
      table.width = gt::pct(90),
      column_labels.border.top.width = gt::px(0),
      table.border.top.width = gt::px(0),
      row_group.border.top.width = gt::px(0),
      row_group.border.bottom.width = gt::px(0)
    ) %>%
    embed_quicksand_or_fallback() %>%
    gt::tab_source_note(
      source_note = gt::html("<div style='text-align:center; color:black;'>* Projections from preseason. Red = Outperforming & Blue = Underperforming.</div>")
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_text(align = "center", color = "black"),
        gt::cell_borders(sides = "bottom", color = "black", weight = gt::px(10))
      ),
      locations = gt::cells_source_notes()
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "bottom", color = "black", weight = gt::px(10)),
      locations = gt::cells_title(groups = "title")
    ) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = Team, rows = Team == "Current Playoff %"),
      fn = function(x) {
        "Current<br>Playoff %"
      }
    ) %>%
    gt::fmt_markdown(
      columns = Team,
      rows = Team == "Current Playoff %"
    ) %>%
    gt::text_transform(
      locations = gt::cells_body(columns = Team, rows = Team == "Projected Playoff %"),
      fn = function(x) {
        "Projected<br>Playoff %"
      }
    ) %>%
    gt::fmt_markdown(
      columns = Team,
      rows = Team == "Projected Playoff %"
    )
  
  for (row_label in c("Wins vs. Projection", "Playoff % vs. Projection")) {
    for (team_col in team_order) {
      gt_table <- gt_table %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#ff9999"),
          locations = gt::cells_body(
            columns = team_col,
            rows = Team == row_label & as.numeric(gsub("[^0-9.-]", "", .data[[team_col]])) > 0
          )
        ) %>%
        gt::tab_style(
          style = gt::cell_fill(color = "#99ccff"),
          locations = gt::cells_body(
            columns = team_col,
            rows = Team == row_label & as.numeric(gsub("[^0-9.-]", "", .data[[team_col]])) < 0
          )
        )
    }
  }
  
  gt_table <- gt_table %>%
    gt::tab_style(
      style = gt::cell_borders(color = "lightgray", sides = c("top", "bottom", "left", "right")),
      locations = gt::cells_body()
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "bottom", color = "black", weight = gt::px(5)),
      locations = gt::cells_column_labels(columns = gt::everything())
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = gt::px(20), weight = "bold"),
      locations = gt::cells_column_labels(columns = gt::everything())
    )
  
  gt_table
}


# =========================== HITTERS BUILDER ==========================

get_color <- function(diff) {
  dplyr::case_when(
    diff >= 20  ~ "#ff6666",
    diff >= 11  ~ "#ff9999",
    diff >= 1   ~ "#ffcccc",
    diff <= -20 ~ "#66b2ff",
    diff <= -11 ~ "#99ccff",
    diff <= -1  ~ "#cce5ff",
    TRUE        ~ "white"
  )
}

apply_cell_fill <- function(gt_table, column, colors) {
  for (i in seq_along(colors)) {
    gt_table <- gt_table %>%
      gt::tab_style(
        style = gt::cell_fill(color = colors[i]),
        locations = gt::cells_body(columns = dplyr::all_of(column), rows = i)
      )
  }
  gt_table
}

add_ellipsis_to_column <- function(gt_table, column, max_width_px = 160) {
  col_quo <- rlang::enquo(column)
  gt_table %>%
    gt::text_transform(
      locations = gt::cells_body(columns = !!col_quo),
      fn = function(x) {
        vapply(
          x,
          function(name) {
            gt::html(paste0(
              "<span style='display:inline-block; max-width:", max_width_px,
              "px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;'>",
              name,
              "</span>"
            ))
          },
          FUN.VALUE = gt::html("")
        )
      }
    )
}

build_hitters_table <- function(df_input, league = "AL", division = "W") {
  Division <- df_input %>% dplyr::filter(League == league, Division == division)
  wide <- Division %>%
    dplyr::select(Team, Player, Range, AVG, OBP, SLG, wOBA, `wRC+`, `K%`, `BB%`, WAR, Games) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      names_from = Range,
      values_from = c(AVG, OBP, SLG, wOBA, `wRC+`, `K%`, `BB%`, WAR, Games),
      names_sep = "_"
    )
  logos <- get_logo_map()
  formatted <- wide %>%
    dplyr::mutate(Logo = logos[Team], .before = Team) %>%
    dplyr::mutate(Team = Logo) %>%
    dplyr::mutate(
      AVG_diff     = 100 * (AVG_Current - AVG_Projected) / AVG_Projected,
      OBP_diff     = 100 * (OBP_Current - OBP_Projected) / OBP_Projected,
      SLG_diff     = 100 * (SLG_Current - SLG_Projected) / SLG_Projected,
      wOBA_diff    = 100 * (wOBA_Current - wOBA_Projected) / wOBA_Projected,
      K_diff       = 100 * (`K%_Projected` - `K%_Current`) / `K%_Projected`,
      BB_diff      = 100 * (`BB%_Current` - `BB%_Projected`) / `BB%_Projected`,
      wRC_diff     = 100 * (`wRC+_Current` - `wRC+_Projected`) / `wRC+_Projected`,
      WAR_adj_diff = 100 * (((WAR_Current / Games_Current) * Games_Projected) - WAR_Projected) / abs(WAR_Projected),
      AVG_color    = get_color(AVG_diff),
      OBP_color    = get_color(OBP_diff),
      SLG_color    = get_color(SLG_diff),
      wOBA_color   = get_color(wOBA_diff),
      K_color      = get_color(K_diff),
      BB_color     = get_color(BB_diff),
      wRC_color    = get_color(wRC_diff),
      WAR_color    = get_color(WAR_adj_diff)
    )
  
  gt_table <- formatted %>%
    dplyr::select(
      Team,
      Player,
      AVG_Current,
      OBP_Current,
      SLG_Current,
      `K%_Current`,
      `BB%_Current`,
      wOBA_Current,
      `wRC+_Current`,
      WAR_Current
    ) %>%
    gt::gt() %>%
    gt::text_transform(
      locations = gt::cells_body(columns = Team),
      fn = function(x) {
        vapply(
          x,
          function(p) {
            if (!is_http_url(p) && file.exists(p)) {
              gt::local_image(filename = p, height = 30)
            } else {
              gt::web_image(url = p, height = 30)
            }
          },
          FUN.VALUE = gt::html("")
        )
      }
    ) %>%
    gt::cols_label(
      AVG_Current   = "AVG",
      OBP_Current   = "OBP",
      SLG_Current   = "SLG",
      wOBA_Current  = "wOBA",
      `wRC+_Current` = "wRC+",
      `K%_Current`  = "K%",
      `BB%_Current` = "BB%",
      WAR_Current   = "WAR"
    ) %>%
    gt::cols_width(Player ~ gt::px(180)) %>%
    gt::cols_align(align = "center", columns = gt::everything()) %>%
    gt::tab_header(
      title = gt::html("<span style='font-size: 32px; font-weight: bold; color: white;'>Current Batting Stats</span>"),
      subtitle = "Colored by Performance vs Projection (WAR adjusted per game)"
    ) %>%
    embed_quicksand_or_fallback() %>%
    gt::tab_style(
      style = list(
        gt::cell_text(color = "white", weight = "bold"),
        gt::cell_fill(color = "black")
      ),
      locations = gt::cells_title(groups = c("title", "subtitle"))
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_text(color = "white", weight = "bold"),
        gt::cell_fill(color = "black")
      ),
      locations = gt::cells_column_labels(columns = gt::everything())
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = gt::px(18)),
      locations = gt::cells_column_labels()
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = gt::px(18)),
      locations = gt::cells_body()
    ) %>%
    gt::tab_options(
      heading.align = "center",
      table.align = "center",
      data_row.padding = gt::px(12),
      table_body.hlines.color = "lightgray"
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "bottom", color = "red", weight = gt::px(5)),
      locations = gt::cells_title(groups = "subtitle")
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "all", color = "lightgray", weight = gt::px(1)),
      locations = gt::cells_body(columns = gt::everything())
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "right", color = "transparent", weight = gt::px(1)),
      locations = gt::cells_body(columns = Team)
    ) %>%
    gt::fmt_number(
      columns = c(AVG_Current, OBP_Current, SLG_Current, wOBA_Current),
      decimals = 3
    ) %>%
    gt::fmt_number(
      columns = c(`K%_Current`, `BB%_Current`, WAR_Current),
      decimals = 1
    ) %>%
    gt::fmt_number(
      columns = `wRC+_Current`,
      decimals = 0
    )
  
  gt_table <- gt_table %>%
    apply_cell_fill("AVG_Current", formatted$AVG_color) %>%
    apply_cell_fill("OBP_Current", formatted$OBP_color) %>%
    apply_cell_fill("SLG_Current", formatted$SLG_color) %>%
    apply_cell_fill("wOBA_Current", formatted$wOBA_color) %>%
    apply_cell_fill("wRC+_Current", formatted$wRC_color) %>%
    apply_cell_fill("K%_Current", formatted$K_color) %>%
    apply_cell_fill("BB%_Current", formatted$BB_color) %>%
    apply_cell_fill("WAR_Current", formatted$WAR_color) %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = list(
        gt::cells_title(groups = c("title", "subtitle")),
        gt::cells_column_labels(),
        gt::cells_body(),
        gt::cells_stub(),
        gt::cells_row_groups()
      )
    ) %>%
    gt::tab_source_note(
      source_note = gt::html(paste0(
        "<div style='text-align:center; font-weight:bold;'>",
        "<strong style='font-size:14px;'>Legend (Difference from Projection):</strong><br>",
        "<table style='margin:auto; border-collapse:separate; border-spacing:2px;'>",
        "<tr>",
        "<td style='background-color:#66b2ff; width:90px; height:18px; font-size:12px;'>&lt; −20%</td>",
        "<td style='background-color:#99ccff; width:90px; height:18px; font-size:12px;'>−19% to −11%</td>",
        "<td style='background-color:#cce5ff; width:90px; height:18px; font-size:12px;'>−10% to −1%</td>",
        "<td style='background-color:white;    width:90px; height:18px; font-size:12px;'>0%</td>",
        "<td style='background-color:#ffcccc;  width:90px; height:18px; font-size:12px;'>+1% to +10%</td>",
        "<td style='background-color:#ff9999;  width:90px; height:18px; font-size:12px;'>+11% to +19%</td>",
        "<td style='background-color:#ff6666;  width:90px; height:18px; font-size:12px;'>&gt; +20%</td>",
        "</tr>",
        "</table>",
        "</div>"
      ))
    )
  
  gt_table <- gt_table %>% add_ellipsis_to_column(Player, max_width_px = 160)
  gt_table
}


# =========================== PITCHERS BUILDER =========================

get_color_pitch <- function(diff, better_lower = FALSE) {
  if (better_lower) diff <- -diff
  dplyr::case_when(
    diff >= 20  ~ "#ff6666",
    diff >= 11  ~ "#ff9999",
    diff >= 1   ~ "#ffcccc",
    diff <= -20 ~ "#66b2ff",
    diff <= -11 ~ "#99ccff",
    diff <= -1  ~ "#cce5ff",
    TRUE        ~ "white"
  )
}

build_pitchers_table <- function(df_input, league = "AL", division = "E") {
  Division_Filter <- df_input %>% dplyr::filter(League == league, Division == division)
  Wide_Table <- Division_Filter %>%
    dplyr::select(Team, Player, Range, ERA, FIP, `K%`, `BB%`, WHIP, WAR, Games) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      names_from = Range,
      values_from = c(ERA, FIP, `K%`, `BB%`, WHIP, WAR, Games),
      names_sep = "_"
    )
  logos <- get_logo_map()
  Formatted_Table <- Wide_Table %>%
    dplyr::mutate(Logo = logos[Team], .before = Team) %>%
    dplyr::mutate(Team = Logo) %>%
    dplyr::mutate(
      ERA_diff              = 100 * (ERA_Current - ERA_Projected) / ERA_Projected,
      FIP_diff              = 100 * (FIP_Current - FIP_Projected) / FIP_Projected,
      K_diff                = 100 * ((`K%_Current` - `K%_Projected`) / `K%_Projected`),
      BB_diff               = 100 * ((`BB%_Current` - `BB%_Projected`) / `BB%_Projected`),
      WHIP_diff             = 100 * (WHIP_Current - WHIP_Projected) / WHIP_Projected,
      WAR_diff              = 100 * (WAR_Current - WAR_Projected) / WAR_Projected,
      WAR_per_game_current  = WAR_Current / Games_Current,
      WAR_per_game_projected = WAR_Projected / Games_Projected,
      Adjusted_WAR_diff     = 100 * (WAR_per_game_current - WAR_per_game_projected) / WAR_per_game_projected,
      ERA_color             = get_color_pitch(ERA_diff,  better_lower = TRUE),
      FIP_color             = get_color_pitch(FIP_diff,  better_lower = TRUE),
      K_color               = get_color_pitch(K_diff,    better_lower = FALSE),
      BB_color              = get_color_pitch(BB_diff,   better_lower = TRUE),
      WHIP_color            = get_color_pitch(WHIP_diff, better_lower = TRUE),
      WAR_color             = get_color_pitch(Adjusted_WAR_diff, better_lower = FALSE)
    )
  
  gt_table <- Formatted_Table %>%
    dplyr::select(
      Team,
      Player,
      ERA_Current,
      FIP_Current,
      `K%_Current`,
      `BB%_Current`,
      WHIP_Current,
      WAR_Current
    ) %>%
    gt::gt() %>%
    gt::text_transform(
      locations = gt::cells_body(columns = Team),
      fn = function(x) {
        vapply(
          x,
          function(p) {
            if (!is_http_url(p) && file.exists(p)) {
              gt::local_image(filename = p, height = 30)
            } else {
              gt::web_image(url = p, height = 30)
            }
          },
          FUN.VALUE = gt::html("")
        )
      }
    ) %>%
    gt::cols_label(
      ERA_Current  = "ERA",
      FIP_Current  = "FIP",
      `K%_Current` = "K%",
      `BB%_Current` = "BB%",
      WHIP_Current = "WHIP",
      WAR_Current  = "WAR"
    ) %>%
    gt::cols_align(align = "center", columns = gt::everything()) %>%
    gt::cols_width(Player ~ gt::px(205)) %>%
    gt::tab_header(
      title = gt::html("<span style='font-size: 32px; font-weight: bold; color: white;'>Current Pitching Stats</span>"),
      subtitle = "Colored by Performance vs Projection"
    ) %>%
    embed_quicksand_or_fallback() %>%
    gt::tab_style(
      style = list(
        gt::cell_text(color = "white", weight = "bold"),
        gt::cell_fill(color = "black")
      ),
      locations = gt::cells_title(groups = c("title", "subtitle"))
    ) %>%
    gt::tab_style(
      style = list(
        gt::cell_text(color = "white", weight = "bold"),
        gt::cell_fill(color = "black")
      ),
      locations = gt::cells_column_labels(columns = gt::everything())
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = gt::px(18)),
      locations = gt::cells_column_labels()
    ) %>%
    gt::tab_style(
      style = gt::cell_text(size = gt::px(18)),
      locations = gt::cells_body()
    ) %>%
    gt::tab_options(
      heading.align = "center",
      table.align = "center",
      data_row.padding = gt::px(12),
      table_body.hlines.color = "lightgray"
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "bottom", color = "red", weight = gt::px(5)),
      locations = gt::cells_title(groups = "subtitle")
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "all", color = "lightgray", weight = gt::px(1)),
      locations = gt::cells_body(columns = gt::everything())
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(sides = "right", color = "transparent", weight = gt::px(1)),
      locations = gt::cells_body(columns = Team)
    ) %>%
    gt::fmt_number(
      columns = c(ERA_Current, FIP_Current, WHIP_Current),
      decimals = 2
    ) %>%
    gt::fmt_number(
      columns = c(`K%_Current`, `BB%_Current`, WAR_Current),
      decimals = 1
    )
  
  for (spec in list(
    list("ERA_Current",  Formatted_Table$ERA_color),
    list("FIP_Current",  Formatted_Table$FIP_color),
    list("K%_Current",   Formatted_Table$K_color),
    list("BB%_Current",  Formatted_Table$BB_color),
    list("WHIP_Current", Formatted_Table$WHIP_color),
    list("WAR_Current",  Formatted_Table$WAR_color)
  )) {
    gt_table <- apply_cell_fill(gt_table, spec[[1]], spec[[2]])
  }
  
  gt_table <- gt_table %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = list(
        gt::cells_title(groups = c("title", "subtitle")),
        gt::cells_column_labels(),
        gt::cells_body(),
        gt::cells_stub(),
        gt::cells_row_groups()
      )
    ) %>%
    gt::tab_source_note(
      source_note = gt::html(paste0(
        "<div style='text-align:center; font-weight:bold;'>",
        "<strong style='font-size:14px;'>Legend (Difference from Projection):</strong><br>",
        "<table style='margin:auto; border-collapse:separate; border-spacing:2px;'>",
        "<tr>",
        "<td style='background-color:#66b2ff; width:90px; height:18px; font-size:12px;'>&lt; −20%</td>",
        "<td style='background-color:#99ccff; width:90px; height:18px; font-size:12px;'>−19% to −11%</td>",
        "<td style='background-color:#cce5ff; width:90px; height:18px; font-size:12px;'>−10% to −1%</td>",
        "<td style='background-color:white;    width:90px; height:18px; font-size:12px;'>0%</td>",
        "<td style='background-color:#ffcccc;  width:90px; height:18px; font-size:12px;'>+1% to +10%</td>",
        "<td style='background-color:#ff9999;  width:90px; height:18px; font-size:12px;'>+11% to +19%</td>",
        "<td style='background-color:#ff6666;  width:90px; height:18px; font-size:12px;'>&gt; +20%</td>",
        "</tr>",
        "</table>",
        "</div>"
      ))
    ) %>%
    add_ellipsis_to_column(Player, max_width_px = 185)
  
  gt_table
}


# ============================ RENDER WRAPPER ==========================

run_with_excel <- function(input_xlsx,
                           sheet,
                           league,
                           division,
                           kind = "teams",
                           output_png,
                           output_html,
                           baseline = NA_character_) {
  deterministic_session()
  if (!file.exists(input_xlsx)) stop(sprintf("Excel not found: %s", input_xlsx))
  ensure_parent_dir(output_png)
  ensure_parent_dir(output_html)
  df <- readxl::read_excel(input_xlsx, sheet = sheet, .name_repair = "minimal")
  
  if (tolower(kind) %in% c("teams", "standings")) {
    tbl <- build_teams_table(df, league = league, division = division)
    target_w <- 1774
    target_h <- 1696
  } else if (tolower(kind) == "hitters") {
    tbl <- build_hitters_table(df, league = league, division = division)
    target_w <- 1366
    target_h <- 954
  } else if (tolower(kind) == "pitchers") {
    tbl <- build_pitchers_table(df, league = league, division = division)
    target_w <- 1366
    target_h <- 954
  } else {
    stop("Unknown kind.")
  }
  
  gt::gtsave(tbl, filename = output_html)
  ensure_png_backend()
  R.utils::withTimeout(
    {
      gt::gtsave(tbl, filename = output_png)
    },
    timeout = 60,
    onTimeout = "error"
  )
  
  normalize_png(
    in_path = output_png,
    out_path = output_png,
    target_w = target_w,
    target_h = target_h,
    gravity = "north",
    color = "white",
    trim_white = TRUE,
    trim_fuzz = "3%",
    preserve_edges = TRUE
  )
  add_inner_keyline(output_png, color = "black", lwd = 1)
  
  backend <- detect_png_backend()
  kind_name <- tools::toTitleCase(if (tolower(kind) == "teams") "teams" else kind)
  message(sprintf(
    "✅ Done [%s].\n  PNG:  %s\n  HTML: %s\n  Backend: %s",
    kind_name,
    normalizePath(output_png,  FALSE),
    normalizePath(output_html, FALSE),
    backend
  ))
  
  if (!is.na(baseline)) maybe_check_html_baseline(output_html, baseline)
  
  backend <- detect_png_backend()
  kind_name <- tools::toTitleCase(if (tolower(kind) == "teams") "teams" else kind)
  message(sprintf(
    "✅ Done [%s].\n  PNG:  %s\n  HTML: %s\n  Backend: %s",
    kind_name,
    normalizePath(output_png,  FALSE),
    normalizePath(output_html, FALSE),
    backend
  ))
  
  invisible(tbl)
}
