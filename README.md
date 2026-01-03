# mlb_x_factor_project — MLB X Factor Update Graphics

Generate polished **teams / hitters / pitchers** tables from Excel with fully reproducible **HTML + PNG** outputs. Deterministic rendering, offline fonts/logos, and optional baseline drift checks.

---

## Table of Contents
- [Quick Start](#quick-start)
- [Requirements](#requirements)
- [Project Structure](#project-structure)
- [Diffs from Original Scripts](#diffs-from-original-scripts)
- [Running](#running)
- [Baselines (Drift Detection)](#baselines-drift-detection)
- [Fonts & Logos](#fonts--logos)
- [Troubleshooting](#troubleshooting)
- [License](#license)

---

## Quick Start

```r
# one-time per machine
source("bootstrap.R")          # renv restore, backend hints, preflight.R (sanity checks - ✅ / ❌ with fixes)

# interactive runner (menus: kind/scope/sheet + baselines)
source("render_x_factor_update_graphics.R")
```

---

## Requirements

- **R 4.3.x** (matches `renv.lock`).
- **Packages** (installed via `renv::restore()`):  
  `dplyr`, `purrr`, `tidyr`, `gt`, `readxl`, `glue`, `withr`, `R.utils`, `magick`, `rlang`, `digest`  
  Optional: `base64enc` (offline Quicksand embedding)
- **PNG backend** (one of):  
  Recommended: `chromote` (+ local Chrome/Chromium)  
  Alternative: `webshot2` (PhantomJS optional)

macOS may require Command Line Tools for source builds:
```bash
xcode-select --install
```

---

## Project Structure

```
create_x_factor_update_graphics.R         # core engines + run_with_excel()
render_x_factor_update_graphics.R         # interactive runner (menus + baselines)

bootstrap.R                               # one-time setup (renv, backend hints, preflight)
preflight.R                               # local checks & guidance

mlb_x_factor_project.Rproj                # RStudio project
.Rprofile                                 # auto-activates renv
renv.lock
renv/
├─ activate.R
└─ settings.json

assets/
├─ mlb/
│  └─ <team>.png                          # ari.png, wsh.png, ...
└─ quicksand_font/
   ├─ Quicksand-Regular.woff2 / .ttf
   ├─ Quicksand-Bold.woff2    / .ttf
   └─ Quicksand-VariableFont_wght.ttf

data/
├─ team_data_x_factor_update.xlsx
├─ hitter_data_x_factor_update.xlsx
└─ pitcher_data_x_factor_update.xlsx

build/
├─ baselines/
│  └─ baseline_<kind>_<league>_<division>_<period>.txt
└─ diff/
   ├─ INDEX.md
   ├─ teams_changelog.md / teams_side_by_side.html
   ├─ hitters_changelog.md / hitters_side_by_side.html
   └─ pitchers_changelog.md / pitchers_side_by_side.html

outputs/                                  # rendered PNG/HTML (git-ignored)
```

---

## Diffs from Original Scripts

This repo includes **curated narrative diffs** and optional **side-by-side HTML** views. They’re easier to read than raw GitHub compares.

- **Start here:** `build/diff/INDEX.md`
- **Teams:** `build/diff/teams_changelog.md`  (HTML: `build/diff/teams_side_by_side.html`)
- **Hitters:** `build/diff/hitters_changelog.md`  (HTML: `build/diff/hitters_side_by_side.html`)
- **Pitchers:** `build/diff/pitchers_changelog.md`  (HTML: `build/diff/pitchers_side_by_side.html`)

The **original code** is stored in a separate branch (`Original---7.10.25`).
> Diff scripts are intentionally **not** in `main` branch. They can be shared on request or hosted as a gist.

### Highlights (Original → Current)
- **Unified, parameterized engine** for league/division/sheet (no hard-coding).
- **Menu-driven runner** with per-sheet baselines.
- **Deterministic session** (locale/timezone/seed) for identical renders.
- **Offline-stable assets** (local logos; Quicksand font embedding).
- **PNG normalization** to a fixed canvas + **inner keyline**.
- **Modern PNG backends** (`chromote` or `webshot2`) with **timeouts**.
- **Consistent output naming**: `outputs/<kind>/<kind>_<period>/…`
- **Canonical baselines**: `build/baselines/baseline_<kind>_<league>_<division>_<period>.txt` (lower_snake_case).

---

## Running

### Interactive (recommended)
```r
source("render_x_factor_update_graphics.R")
```
You choose:
- **Kind:** teams / hitters / pitchers / ALL
- **Scope:** single division (AL/NL × W/E/C) or all divisions
- **Sheet:** timestamp snapshot
- **Baselines:** enforce and/or write per kind x league x division x sheet

**Outputs**
```
outputs/<kind>/<kind>_<period>/
  <league>_<division>_<kind>_<period>.html
  <league>_<division>_<kind>_<period>.png
```
Examples:
```
outputs/teams/teams_july_2025/al_w_teams_july_2025.png
outputs/hitters/hitters_eos_2025/nl_c_hitters_eos_2025.html
```

### Direct call (advanced)
```r
source("create_x_factor_update_graphics.R")
run_with_excel(
  input_xlsx  = "data/team_data_x_factor_update.xlsx",
  sheet       = "July 2025",
  league      = "AL", division = "W",
  kind        = "teams",
  output_png  = "outputs/teams/teams_july_2025/al_w_teams_july_2025.png",
  output_html = "outputs/teams/teams_july_2025/al_w_teams_july_2025.html",
  baseline    = NA_character_  # or a path under build/baselines/...
)
```

---

## Baselines (Drift Detection)

Baselines live under `build/baselines/` and are **per kind × league × division × sheet**:

```
baseline_<kind>_<league>_<division>_<period>.txt
# e.g., baseline_teams_al_w_july_2025.txt
```

- **Read (back-compat):** legacy uppercase/old patterns are detected.
- **Write (canonical):** filenames are always lower_snake_case.
- **Normalize existing files (optional):**
  ```r
  normalize_baselines_now <- function(dir = "build/baselines") {
    if (!dir.exists(dir)) stop("Baselines dir not found: ", dir)
    files <- list.files(dir, pattern = "^baseline_.*\.txt$", full.names = TRUE)
    for (f in files) {
      bn <- basename(f)
      bn_new <- tolower(bn); bn_new <- gsub("_+", "_", bn_new); bn_new <- gsub("^_|_$", "", bn_new)
      if (!identical(bn_new, bn)) file.rename(f, file.path(dirname(f), bn_new))
    }
    invisible(TRUE)
  }
  # normalize_baselines_now("build/baselines")
  ```

---

## Fonts & Logos

- **Team logos:** `assets/mlb/*.png` preferred; ESPN CDN fallback if missing.
- **Quicksand font:** `assets/quicksand_font/` (Regular/Bold/Variable). If present **and** `base64enc` installed, HTML embeds fonts for offline use; otherwise Google Fonts fallback.

---

## Troubleshooting

- **PNG export error:** install `chromote` (preferred) or `webshot2` (optionally `webshot2::install_phantomjs()`).
- **Baseline drift error:** if intentional, update baseline via runner prompt; otherwise investigate inputs/assets/versions.
- **Fonts differ across machines:** ensure Quicksand files exist locally (or accept Google Fonts fallback).
- **macOS source build errors:** `xcode-select --install`.

---

## License

See [LICENSE](./LICENSE).
