# Pitchers — Narrative Change Summary

- Original: `original/X Factor Update Pitcher Code - 7.10.25.R`
- Current:  `create_x_factor_update_graphics.R`

## Size & Structure
- Lines: 228 → 1019 (nonblank: 207 → 936; comments: 17 → 23)
- Functions: 3 → 31 (avg len: 61 → 32.1, max len: 135 → 249)

## Package Changes
- Added:
  - base64enc (current v0.1-3)
  - chromote (current v0.5.1)
  - digest (current v0.6.37)
  - glue (current v1.8.0)
  - magick (current v2.8.6)
  - purrr (current v1.0.4)
  - R.utils (current v2.13.0)
  - readxl (current v1.4.5)
  - webshot (current v0.5.5)
  - withr (current v3.0.2)
- Removed:
  - stringr
  - sysfonts
- Unchanged:
  - dplyr (current v1.1.4)
  - gt (current v1.0.0)
  - rlang (current v1.1.6)
  - tidyr (current v1.3.1)
  - webshot2 (current v0.1.2)

## Capabilities — New
- [x] Parameterized league/division (no hard-coded divisions)
- [x] Deterministic session (locale, timezone, seed)
- [x] Baseline hashing & drift enforcement
- [x] Local logos (offline) with URL fallback
- [x] Local Quicksand font embedding (offline)
- [x] Chromote backend support
- [x] Webshot/Webshot2 support
- [x] PNG normalization to exact canvas
- [x] Inner keyline border
- [x] Render timeout protection
- [x] Menu-driven runner / sheet picker

## Capabilities — Retained
- (none)

## Capabilities — Removed
- (none)

## Notable Hard-coding Removed / Safer Patterns
- [x] Removed hard-coded League/Division constants
- [x] Replaced ESPN-only logos with local+fallback strategy

## Rendering & Assets (Signals found)
- Original: espn_logos, google_font_quicksand, hardcoded_division
- Current:  deterministic_session, renv_integration, baseline_hashing, local_logos, espn_logos, google_font_quicksand, base64_font_embed, chromote_usage, webshot_usage, png_normalization, keyline, timeout_enforced, parameterized_league_div, menu_runner

## Notes
- Feature presence is detected via fixed-string probes (no regex).
- For exact line-by-line changes, open the side-by-side HTML diff or GitHub Compare.
