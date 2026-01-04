# Install required packages (if not already installed)
install.packages(c("dplyr", "gt", "tidyr", "webshot2", "sysfonts"))
install.packages("stringr")
install.packages("rlang")

# Load libraries
library(dplyr)
library(tidyr)
library(gt)
library(webshot2)
library(sysfonts)
library(purrr)
library(stringr)
library(rlang)

# Data setup
xfactordata <- X_Factor_Update_July_Hitters

# Filter for NL West
Division <- xfactordata %>%
  filter(League == "AL", Division == "W")

# Pivot to wide format
wide <- Division %>%
  select(Team, Player, Range, AVG, OBP, SLG, wOBA, `wRC+`, `K%`, `BB%`, WAR, Games) %>%
  distinct() %>%
  pivot_wider(
    names_from = Range,
    values_from = c(AVG, OBP, SLG, wOBA, `wRC+`, `K%`, `BB%`, WAR, Games),
    names_sep = "_"
  )

# Team logos from ESPN
team_logos <- tibble::tibble(
  Team = c("ATL", "NYM", "PHI", "MIA", "WSN", "CHC", "MIL", "CIN", "STL", "PIT",
           "LAD", "SFG", "SDP", "ARI", "COL", "HOU", "SEA", "TEX", "LAA", "ATH",
           "CLE", "MIN", "DET", "KCR", "CHW", "NYY", "BOS", "TOR", "TBR", "BAL"),
  Logo = paste0("https://a.espncdn.com/i/teamlogos/mlb/500/",
                tolower(c("ATL", "NYM", "PHI", "MIA", "WSH", "CHC", "MIL", "CIN", "STL", "PIT",
                          "LAD", "SF", "SD", "ARI", "COL", "HOU", "SEA", "TEX", "LAA", "OAK",
                          "CLE", "MIN", "DET", "KC", "CHW", "NYY", "BOS", "TOR", "TB", "BAL")), 
                ".png")
)

# Join logos
formatted <- wide %>%
  left_join(team_logos, by = "Team") %>%
  mutate(Team = Logo)

# Updated color scale function with expanded thresholds
get_color <- function(diff) {
  case_when(
    diff >= 20 ~ "#ff6666",
    diff >= 11 ~ "#ff9999",
    diff >= 1 ~ "#ffcccc",
    diff <= -20 ~ "#66b2ff",
    diff <= -11 ~ "#99ccff",
    diff <= -1 ~ "#cce5ff",
    TRUE ~ "white"
  )
}

# Add % difference + color columns
formatted <- formatted %>%
  mutate(
    AVG_diff = 100 * (AVG_Current - AVG_Projected) / AVG_Projected,
    OBP_diff = 100 * (OBP_Current - OBP_Projected) / OBP_Projected,
    SLG_diff = 100 * (SLG_Current - SLG_Projected) / SLG_Projected,
    wOBA_diff = 100 * (wOBA_Current - wOBA_Projected) / wOBA_Projected,
    K_diff = 100 * (`K%_Projected` - `K%_Current`) / `K%_Projected`,
    BB_diff = 100 * (`BB%_Current` - `BB%_Projected`) / `BB%_Projected`,
    wRC_diff = 100 * (`wRC+_Current` - `wRC+_Projected`) / `wRC+_Projected`,
    WAR_adj_diff = 100 * (((WAR_Current / Games_Current) * Games_Projected) - WAR_Projected) / abs(WAR_Projected),
    
    AVG_color = get_color(AVG_diff),
    OBP_color = get_color(OBP_diff),
    SLG_color = get_color(SLG_diff),
    wOBA_color = get_color(wOBA_diff),
    K_color = get_color(K_diff),
    BB_color = get_color(BB_diff),
    wRC_color = get_color(wRC_diff),
    WAR_color = get_color(WAR_adj_diff)
  )

apply_cell_fill <- function(gt_table, column, colors) {
  for (i in seq_along(colors)) {
    gt_table <- gt_table %>%
      tab_style(
        style = cell_fill(color = colors[i]),
        locations = cells_body(columns = all_of(column), rows = i)
      )
  }
  gt_table
}

# Build gt table with reordered columns
gt_table <- formatted %>%
  select(
    Team, Player,
    AVG_Current, OBP_Current, SLG_Current,
    `K%_Current`, `BB%_Current`,
    wOBA_Current, `wRC+_Current`, WAR_Current
  ) %>%
  gt() %>%
  text_transform(
    locations = cells_body(columns = Team),
    fn = function(x) web_image(url = x, height = 30)
  ) %>%
  cols_label(
    AVG_Current = "AVG", OBP_Current = "OBP", SLG_Current = "SLG",
    wOBA_Current = "wOBA", `wRC+_Current` = "wRC+",
    `K%_Current` = "K%", `BB%_Current` = "BB%", WAR_Current = "WAR"
  ) %>%
  cols_width(
    Player ~ px(180)  
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_header(
    title = html("<span style='font-size: 32px; font-weight: bold; color: white;'>Current Batting Stats</span>"),
    subtitle = "Colored by Performance vs Projection (WAR adjusted per game)"
  ) %>%
  opt_table_font(font = list(gt::google_font("Quicksand"), default_fonts())) %>%
  tab_style(
    style = list(cell_text(color = "white", weight = "bold"), cell_fill(color = "black")),
    locations = cells_title(groups = c("title", "subtitle"))
  ) %>%
  tab_style(
    style = list(cell_text(color = "white", weight = "bold"), cell_fill(color = "black")),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(style = cell_text(size = px(18)), locations = cells_column_labels()) %>%
  tab_style(style = cell_text(size = px(18)), locations = cells_body()) %>%
  tab_options(
    heading.align = "center",
    table.align = "center",
    data_row.padding = px(12),
    table_body.hlines.color = "lightgray"
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "red", weight = px(5)),
    locations = cells_title(groups = "subtitle")
  ) %>%
  tab_style(
    style = cell_borders(sides = "all", color = "lightgray", weight = px(1)),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_borders(sides = "right", color = "transparent", weight = px(1)),
    locations = cells_body(columns = Team)
  ) %>%
  fmt_number(columns = c(AVG_Current, OBP_Current, SLG_Current, wOBA_Current), decimals = 3) %>%
  fmt_number(columns = c(`K%_Current`, `BB%_Current`, WAR_Current), decimals = 1) %>%
  fmt_number(columns = `wRC+_Current`, decimals = 0)

# Apply colors
gt_table <- gt_table %>%
  apply_cell_fill("AVG_Current", formatted$AVG_color) %>%
  apply_cell_fill("OBP_Current", formatted$OBP_color) %>%
  apply_cell_fill("SLG_Current", formatted$SLG_color) %>%
  apply_cell_fill("wOBA_Current", formatted$wOBA_color) %>%
  apply_cell_fill("wRC+_Current", formatted$wRC_color) %>%
  apply_cell_fill("K%_Current", formatted$K_color) %>%
  apply_cell_fill("BB%_Current", formatted$BB_color) %>%
  apply_cell_fill("WAR_Current", formatted$WAR_color)

# Bold Font
gt_table <- gt_table %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_title(groups = c("title", "subtitle")),
      cells_column_labels(),
      cells_body(),
      cells_stub(),
      cells_row_groups()
    )
  )

# Add legend
gt_table <- gt_table %>%
  tab_source_note(
    source_note = html(
      paste0(
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
      )
    )
  )

add_ellipsis_to_column <- function(gt_table, column, max_width_px = 160) {
  col_quo <- rlang::enquo(column)
  
  gt_table %>%
    gt::text_transform(
      locations = gt::cells_body(columns = !!col_quo),
      fn = function(x) {
        vapply(x, function(name) {
          gt::html(paste0(
            "<span style='display:inline-block; max-width:", max_width_px, 
            "px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;'>",
            name,
            "</span>"
          ))
        }, FUN.VALUE = gt::html(""))
      }
    )
}

gt_table <- gt_table %>%
  add_ellipsis_to_column(Player, max_width_px = 160)

gt_table

# Save table
gtsave(gt_table, filename = "~/Desktop/July X Factor Update - Hitters ALW.png")
gtsave(gt_nlw, filename = "~/Desktop/current_stats_table.html")
