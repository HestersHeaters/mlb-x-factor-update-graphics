# Load libraries
library(dplyr)
library(tidyr)
library(gt)
library(webshot2)
library(sysfonts)
library(stringr)
library(rlang)

# Data setup: assumes `X_Factor_Update_July` already exists
xfactordata <- X_Factor_Update_July_Pitchers

# Filter for NL West
Division_Filter <- xfactordata %>%
  filter(League == "AL", Division == "E")

# Pivot to wide format
Wide_Table <- Division_Filter %>%
  select(Team, Player, Range, ERA, FIP, `K%`, `BB%`, WHIP, WAR, Games) %>%
  distinct() %>%
  pivot_wider(
    names_from = Range,
    values_from = c(ERA, FIP, `K%`, `BB%`, WHIP, WAR, Games),
    names_sep = "_"
  )

# Team logos from ESPN
team_logos <- tibble::tibble(
  Team = c("ATL", "NYM", "PHI", "MIA", "WSN", "CHC", "MIL", "CIN", "STL", "PIT",
           "LAD", "SFG", "SDP", "ARI", "COL", "HOU", "SEA", "TEX", "LAA", "ATH",
           "CLE", "MIN", "DET", "KCR", "CHW", "NYY", "BOS", "TOR", "TBR", "BAL"),
  Logo = paste0(
    "https://a.espncdn.com/i/teamlogos/mlb/500/",
    tolower(c("ATL", "NYM", "PHI", "MIA", "WSH", "CHC", "MIL", "CIN", "STL", "PIT",
              "LAD", "SF", "SD", "ARI", "COL", "HOU", "SEA", "TEX", "LAA", "OAK",
              "CLE", "MIN", "DET", "KC", "CHW", "NYY", "BOS", "TOR", "TB", "BAL")),
    ".png")
)

# Join logos & replace team name with image
Formatted_Table <- Wide_Table %>%
  left_join(team_logos, by = "Team") %>%
  mutate(Team = Logo)

# Color scale function
get_color <- function(diff, better_lower = FALSE) {
  if (better_lower) diff <- -diff
  case_when(
    diff >= 20  ~ "#ff6666",
    diff >= 11  ~ "#ff9999",
    diff >= 1   ~ "#ffcccc",
    diff <= -20 ~ "#66b2ff",
    diff <= -11 ~ "#99ccff",
    diff <= -1  ~ "#cce5ff",
    TRUE        ~ "white"
  )
}

# Compute % diffs & colors
Formatted_Table <- Formatted_Table %>%
  mutate(
    ERA_diff   = 100 * (ERA_Current - ERA_Projected) / ERA_Projected,
    FIP_diff   = 100 * (FIP_Current - FIP_Projected) / FIP_Projected,
    K_diff     = 100 * ((`K%_Current` - `K%_Projected`) / `K%_Projected`),
    BB_diff    = 100 * ((`BB%_Current` - `BB%_Projected`) / `BB%_Projected`),
    WHIP_diff  = 100 * (WHIP_Current - WHIP_Projected) / WHIP_Projected,
    WAR_diff   = 100 * (WAR_Current - WAR_Projected) / WAR_Projected,
    
    WAR_per_game_current   = WAR_Current / Games_Current,
    WAR_per_game_projected = WAR_Projected / Games_Projected,
    Adjusted_WAR_diff      = 100 * (WAR_per_game_current - WAR_per_game_projected) / WAR_per_game_projected,
    
    ERA_color  = get_color(ERA_diff, better_lower = TRUE),
    FIP_color  = get_color(FIP_diff, better_lower = TRUE),
    K_color    = get_color(K_diff, better_lower = FALSE),
    BB_color   = get_color(BB_diff, better_lower = TRUE),
    WHIP_color = get_color(WHIP_diff, better_lower = TRUE),
    WAR_color  = get_color(Adjusted_WAR_diff, better_lower = FALSE)
  )

# Helper: apply conditional fill colors
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

# Helper: truncate long names
add_ellipsis_to_column <- function(gt_table, column, max_width_px = 160) {
  col_quo <- rlang::enquo(column)
  gt_table %>%
    text_transform(
      locations = cells_body(columns = !!col_quo),
      fn = function(x) {
        vapply(x, function(name) {
          html(paste0(
            "<span style='display:inline-block; max-width:", max_width_px,
            "px; overflow:hidden; text-overflow:ellipsis; white-space:nowrap;'>",
            name, "</span>"
          ))
        }, FUN.VALUE = html(""))
      }
    )
}

# Build gt table
gt_table <- Formatted_Table %>%
  select(
    Team, Player,
    ERA_Current, FIP_Current, `K%_Current`, `BB%_Current`, WHIP_Current, WAR_Current
  ) %>%
  gt() %>%
  text_transform(
    locations = cells_body(columns = Team),
    fn = function(x) web_image(url = x, height = 30)
  ) %>%
  cols_label(
    ERA_Current = "ERA", FIP_Current = "FIP",
    `K%_Current` = "K%", `BB%_Current` = "BB%",
    WHIP_Current = "WHIP", WAR_Current = "WAR"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_width(
    Player ~ px(205)  
  ) %>%
  tab_header(
    title = html("<span style='font-size: 32px; font-weight: bold; color: white;'>Current Pitching Stats</span>"),
    subtitle = "Colored by Performance vs Projection"
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
  fmt_number(columns = c(ERA_Current, FIP_Current, WHIP_Current), decimals = 2) %>%
  fmt_number(columns = c(`K%_Current`, `BB%_Current`, WAR_Current), decimals = 1)

# Apply conditional fill colors
colors <- list(
  list("ERA_Current",  Formatted_Table$ERA_color),
  list("FIP_Current",  Formatted_Table$FIP_color),
  list("K%_Current",   Formatted_Table$K_color),
  list("BB%_Current",  Formatted_Table$BB_color),
  list("WHIP_Current", Formatted_Table$WHIP_color),
  list("WAR_Current",  Formatted_Table$WAR_color)
)

for (col in colors) {
  gt_table <- apply_cell_fill(gt_table, col[[1]], col[[2]])
}

# Final bold style everywhere
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

# Truncate player names if too long
gt_table <- gt_table %>%
  add_ellipsis_to_column(Player, max_width_px = 185)

# Display table
gt_table

# Save outputs
gtsave(gt_table, filename = "~/Desktop/July X Factor Update - Pitchers ALE.png")
gtsave(gt_nlw_pitchers, filename = "~/Desktop/current_pitching_stats_table.html")


