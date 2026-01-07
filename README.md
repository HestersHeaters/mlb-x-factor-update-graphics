# mlb_x_factor_project — MLB X Factor Update Graphics

Generate polished, presentation-ready **teams / hitters / pitchers** performance tables from Excel with reproducible **HTML + PNG** outputs.  
Built for portability: locked R environment (**renv**), deterministic rendering, offline fonts/logos, and optional baseline drift detection.  
**Run:** one-time `bootstrap.R`, then `render_x_factor_update_graphics.R`.

<!-- Badges -->
[![Restore & Smoke](https://github.com/HestersHeaters/mlb-x-factor-update-graphics/actions/workflows/restore-smoke.yml/badge.svg)](https://github.com/HestersHeaters/mlb-x-factor-update-graphics/actions/workflows/restore-smoke.yml)
![R](https://img.shields.io/badge/R-4.3.x-276DC3?logo=r&logoColor=white)
[![License: PolyForm Noncommercial 1.0.0](https://img.shields.io/badge/License-PolyForm%20Noncommercial%201.0.0-blue.svg)](LICENSE)
![Platforms](https://img.shields.io/badge/platforms-macOS%20%7C%20Windows-lightgrey)
![Made with gt](https://img.shields.io/badge/made%20with-gt-444?logo=r&logoColor=white)
![renv](https://img.shields.io/badge/renv-locked-blue)
![Repo size](https://img.shields.io/github/repo-size/HestersHeaters/mlb-x-factor-update-graphics)

---

## Value Proposition

- **Standardized reporting** across divisions & time snapshots
- **Consistent, presentation-ready visuals** for stakeholders with no manual formatting required
- **Reproducible & portable** across machines (collaboration-ready)
- **At-a-glance interpretability** by color gradient encoding vs projection (red above, blue below)
- **Optional baseline drift checks** to catch unintended visual changes early

--

## Table of Contents

1. [Output Samples](#output-samples)
2. [Quick Start](#quick-start)
3. [Requirements](#requirements)
4. [Running](#running)
5. [Baselines (Drift Detection)](#baselines-drift-detection)
6. [Reproducibility & Portability](#reproducibility--portability)
7. [Project Structure](#project-structure)
8. [Diffs from Original Scripts](#diffs-from-original-scripts)
9. [Reproducing Diffs](#reproducing-diffs)
10. [Troubleshooting](#troubleshooting)
11. [License](#license)

---

## Output Samples

<details>
<summary><strong>Click to expand preview images</strong></summary>

<p>

<!-- Teams -->
<figure>
  <img src="samples/nl_w_teams_eos_2025_sample.png" alt="Teams table — NL West, End of Season 2025" width="900">
  <figcaption><em>Teams — NL West — End of Season 2025</em></figcaption>
</figure>

<!-- Hitters -->
<figure>
  <img src="samples/al_e_hitters_eos_2025_sample.png" alt="Hitters table — AL East, End of Season 2025" width="900">
  <figcaption><em>Hitters — AL East — End of Season 2025</em></figcaption>
</figure>

<!-- Pitchers -->
<figure>
  <img src="samples/nl_c_pitchers_eos_2025_sample.png" alt="Pitchers table — NL Central, End of Season 2025" width="900">
  <figcaption><em>Pitchers — NL Central — End of Season 2025</em></figcaption>
</figure>

</p>
</details>

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

## Running

### Interactive (recommended)
```r
# one-time per machine
source("bootstrap.R")          # renv restore, backend hints, preflight.R (sanity checks - ✅ / ❌ with fixes)

# interactive runner (menus: kind/scope/sheet + baselines)
source("render_x_factor_update_graphics.R")
```
You choose:
- **Kind:** teams / hitters / pitchers / ALL
- **Scope:** single division (AL/NL × W/E/C) or all divisions
- **Sheet:** timestamp snapshot
- **Baselines:** enforce and/or write per kind x league x division x sheet; see **Baselines (Drift Detection)** below for more info

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

### Recommended Workflow

Baselines are included so the **default workflow is to enforce drift checks** (i.e., treat unexpected visual changes as a signal to investigate).  
If you intentionally change data, styling, or rendering behavior, you can **write/update baselines** to accept the new output as the new standard.

### Features

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

## Reproducibility & Portability

- **Environment locking:** Project uses `renv` + `renv.lock`. Opening the repo activates the exact package set for consistent results across machines.
- **Deterministic rendering:** Locale, timezone, and seed are fixed so HTML/PNG outputs are bit‑stable given the same inputs.
- **PNG backend:** Works with `chromote` (recommended) or `webshot2` + PhantomJS. The runner auto‑detects and guides setup.
- **Baseline drift checks (optional):** Per kind/league/division HTML hashes in `build/baselines/` let you catch unintended visual changes quickly.
- **Assets & Typography:** Uses local MLB logos (with ESPN URL fallback) and embeds **Quicksand** locally when `base64enc` is present; otherwise falls back to Google Fonts. This keeps visuals consistent offline.

---

## Project Structure

```
create_x_factor_update_graphics.R         — Core table builders + utilities; exposes `run_with_excel()`  
render_x_factor_update_graphics.R         — Interactive runner (menus, scope, baselines)

bootstrap.R                               — One-time setup: renv restore, backend hints, preflight  
preflight.R                               — Local environment checks with actionable fixes

mlb_x_factor_project.Rproj                — RStudio project (opens with renv auto-activation)  
.Rprofile                                 — Auto-activates renv; pins CRAN mirror and options  
renv.lock                                 — Pinned package versions for reproducibility  
renv/  
├─ activate.R                             — renv bootstrap (auto-run via `.Rprofile`)  
└─ settings.json                          — renv project settings

assets/  
├─ mlb/  
│  └─ <team>.png                          — Local team logos (e.g., `ari.png`, `wsh.png`)  
└─ quicksand_font/  
   ├─ Quicksand-Regular.woff2 / .ttf      — Quicksand Regular for offline embedding  
   ├─ Quicksand-Bold.woff2    / .ttf      — Quicksand Bold for offline embedding  
   └─ Quicksand-VariableFont_wght.ttf     — Variable font fallback

data/  
├─ team_data_x_factor_update.xlsx         — Teams workbook (input)  
├─ hitter_data_x_factor_update.xlsx       — Hitters workbook (input)  
└─ pitcher_data_x_factor_update.xlsx      — Pitchers workbook (input)

build/  
├─ baselines/  
│  └─ baseline_<kind>_<league>_<division>_<sheet>.txt  — HTML hash baselines (drift checks)  
└─ diff/  
   ├─ INDEX.md                             — Diff index with links  
   ├─ teams_changelog.md / teams_side_by_side.html  
   ├─ hitters_changelog.md / hitters_side_by_side.html  
   └─ pitchers_changelog.md / pitchers_side_by_side.html

make_diffs.R                               — One-click driver: writes changelogs + HTML diffs to `build/diff/`  
tools/  
└─ x_factor_diffs.R                        — Diff engine (capability probes, package deltas, HTML side-by-side)

outputs/                                   — Rendered PNG/HTML (git-ignored)

.github/  
└─ workflows/  
   └─ restore-smoke.yml                    — CI: renv restore + syntax smoke test
```

---

## Diffs from Original Scripts

This repo includes **curated narrative diffs** and optional **side-by-side HTML** views. They’re easier to read than raw GitHub compares.

- **Start here:** `build/diff/INDEX.md`
- **Teams:** `build/diff/teams_changelog.md`  (HTML: `build/diff/teams_side_by_side.html`)
- **Hitters:** `build/diff/hitters_changelog.md`  (HTML: `build/diff/hitters_side_by_side.html`)
- **Pitchers:** `build/diff/pitchers_changelog.md`  (HTML: `build/diff/pitchers_side_by_side.html`)

The **original code** is stored in a separate branch (`Original---7.10.25`).

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

## Reproducing Diffs

This repo ships with a small toolchain to generate narrative changelogs and side‑by‑side HTML diffs between the **original** scripts and the **current unified** implementation.

### Option A — Remote originals (no cloning another branch)
Run:
```r
source("make_diffs.R")
```

### Option B — Local originals (offline)
Place files in `original/` with the exact names , then run:
```r
source("make_diffs.R")
```

### Outputs
- Markdown changelogs + HTML diffs are written to: `build/diff/`
  - `teams_changelog.md`, `hitters_changelog.md`, `pitchers_changelog.md`
  - `*_side_by_side.html`
  - `INDEX.md` (quick links)

### Tips
- If HTML diffs don’t render, install `diffobj` (the script will auto‑install if missing).
- If a URL 404s, confirm the **branch name** and **path encoding** in the `base` block.

---

## Troubleshooting

- **PNG export error:** install `chromote` (preferred) or `webshot2` (optionally `webshot2::install_phantomjs()`).
- **Baseline drift error:** if intentional, update baseline via runner prompt; otherwise investigate inputs/assets/versions.
- **Fonts differ across machines:** ensure Quicksand files exist locally (or accept Google Fonts fallback).
- **macOS source build errors:** `xcode-select --install`.

---

## License

See [LICENSE](./LICENSE).
