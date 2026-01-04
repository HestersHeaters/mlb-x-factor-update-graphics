# Complete R code: GT table with conditional formatting, borders, bold/large headers, logos, and Standing row

library(dplyr)
library(gt)
library(purrr)
library(htmltools)
library(webshot2)

# Starting dataset
df <- Team_Records_End_of_Season_2025

# Step 1: Extract League & Division rows
league_row   <- df[1, -1]
division_row <- df[2, -1]
team_names   <- names(df)[-1]

# Step 2: Identify NL West team columns
division_cols <- which(league_row == "NL" & division_row == "W")

# Step 3: Subset stats rows and keep Stat + NL West team columns
filtered_df <- df[-c(1, 2), c(1, division_cols + 1)]
colnames(filtered_df) <- c("Team", team_names[division_cols])

# Step 4: Filter to selected stats + Standing
keep_stats <- c("Standing", "Current Record", "Proj Record", "Wins vs. Proj", "CP %", "Proj Playoff %")
filtered_df <- filtered_df %>% filter(Team %in% keep_stats)

# Step 5: Format Wins vs. Proj
row_index <- which(filtered_df$Team == "Wins vs. Proj")
numeric_vals <- round(as.numeric(unlist(filtered_df[row_index, -1])))
formatted_vals <- ifelse(numeric_vals > 0, paste0("+", numeric_vals), as.character(numeric_vals))
filtered_df[row_index, -1] <- as.list(formatted_vals)

# Step 6: Add Playoff % Difference row
cp_vals <- as.numeric(unlist(df[df[[1]] == "CP %", division_cols + 1]))
proj_vals <- as.numeric(unlist(df[df[[1]] == "Proj Playoff %", division_cols + 1]))
playoff_diff <- round((cp_vals - proj_vals) * 100, 1)
playoff_diff_fmt <- ifelse(playoff_diff > 0, paste0("+", playoff_diff, "%"), paste0(playoff_diff, "%"))
playoff_diff_row <- c("Playoff % Diff", playoff_diff_fmt)
filtered_df <- bind_rows(filtered_df, setNames(as.list(playoff_diff_row), names(filtered_df)))

# Step 7: Format CP % and Proj Playoff %
for (row_label in c("CP %", "Proj Playoff %")) {
  idx <- which(filtered_df$Team == row_label)
  vals <- as.numeric(unlist(filtered_df[idx, -1])) * 100
  formatted <- paste0(round(vals, 1), "%")
  filtered_df[idx, -1] <- as.list(formatted)
}

# Step 8: Reorder rows to put Standing at top
row_order <- c("Standing", "Current Record", "Proj Record", "Wins vs. Proj", "CP %", "Proj Playoff %", "Playoff % Diff")
filtered_df <- filtered_df %>%
  mutate(row_factor = factor(Team, levels = row_order)) %>%
  arrange(row_factor) %>%
  select(-row_factor)

# Step 9: Reorder team columns by Standing rank (ordinal: 1st, 2nd, etc.)
ordinal_to_numeric <- function(x) {
  as.numeric(sub("(st|nd|rd|th)", "", x))
}
standing_row <- filtered_df %>% filter(Team == "Standing") %>% select(-Team)
team_order <- colnames(standing_row)[order(sapply(standing_row[1, ], ordinal_to_numeric))]
filtered_df <- filtered_df[, c("Team", team_order)]

# Step 10: Team logos
mlb_logos <- c(
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
  KCR  = "https://a.espncdn.com/i/teamlogos/mlb/500/kc.png",
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
header_with_logos <- map(team_order, ~html(
  paste0('<div style="text-align:center; font-weight:bold; font-size:20px;">',
         '<img src="', mlb_logos[.x], '" height=50><br><span>', .x, '</span></div>')
)) %>%
  set_names(team_order)

# Step 11: Table Creation
gt_table <- gt(filtered_df %>% mutate(Team = recode(Team,
                                                    "Proj Record" = "Projected Record",
                                                    "Wins vs. Proj" = "Wins vs. Projection",
                                                    "CP %" = "Current Playoff %",
                                                    "Proj Playoff %" = "Projected Playoff %",
                                                    "Playoff % Diff" = "Playoff % vs. Projection"
))) %>%
  tab_header(
    title = md("<div style='font-size:60px; font-weight:bold; color:white;'>Current Team Standings</div>")
  ) %>%
  tab_style(
    style = cell_fill(color = "black"),
    locations = cells_title(groups = "title")
  ) %>%
  tab_style(
    style = list(cell_text(align = "center", weight = "bold", size = px(20))),
    locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(align = "center", weight = "bold", size = px(20)),
    locations = cells_column_labels(columns = everything())
  ) %>%
  cols_width(
    Team ~ px(180),
    everything() ~ px(120)  # wider columns
  ) %>%
  cols_label(.list = header_with_logos) %>%
  tab_options(
    data_row.padding = px(25),         # tighter rows
    heading.padding = px(10),         # tighter heading
    column_labels.padding = px(4),
    table.width = pct(90),            # wider table
    column_labels.border.top.width = px(0),
    table.border.top.width = px(0),
    row_group.border.top.width = px(0),
    row_group.border.bottom.width = px(0)
  ) %>%
  opt_table_font(
    font = google_font(name = "Quicksand"),
    weight = "bold"
  ) %>%
  tab_source_note(
    source_note = html("<div style='text-align:center; color:black;'>* Projections from preseason. Red = Outperforming & Blue = Underperforming.</div>")
  ) %>%
  tab_style(
    style = list(
      cell_text(align = "center", color = "black"),
      cell_borders(sides = "bottom", color = "black", weight = px(10))
    ),
    locations = cells_source_notes()
  ) %>% 
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(10)
    ),
    locations = cells_title(groups = "title")
  ) %>%
  text_transform(
    locations = cells_body(
      columns = Team,
      rows = Team == "Current Playoff %"
    ),
    fn = function(x) {
      "Current<br>Playoff %"
    }
  ) %>%
  fmt_markdown(columns = Team, rows = Team == "Current Playoff %"
  ) %>%
  text_transform(
    locations = cells_body(
      columns = Team,
      rows = Team == "Projected Playoff %"
    ),
    fn = function(x) {
      "Projected<br>Playoff %"
    }
  ) %>%
  fmt_markdown(columns = Team, rows = Team == "Projected Playoff %")
  




# Step 12: Conditional formatting
for (row_label in c("Wins vs. Projection", "Playoff % vs. Projection")) {
  for (team_col in team_order) {
    gt_table <- gt_table %>%
      tab_style(
        style = cell_fill(color = "#ff9999"),
        locations = cells_body(
          columns = team_col,
          rows = Team == row_label & as.numeric(gsub("[^0-9.-]", "", .data[[team_col]])) > 0
        )
      ) %>%
      tab_style(
        style = cell_fill(color = "#99ccff"),
        locations = cells_body(
          columns = team_col,
          rows = Team == row_label & as.numeric(gsub("[^0-9.-]", "", .data[[team_col]])) < 0
        )
      )
  }
}

# Step 13: Add lightgray borders & style column headers
gt_table <- gt_table %>%
  tab_style(
    style = cell_borders(color = "lightgray", sides = c("top", "bottom", "left", "right")),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(5)),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(size = px(20), weight = "bold"),
    locations = cells_column_labels(columns = everything())
  )

gt_table

# Step 14: Save

saveRDS(gt_table, file = "EOS Team Update NLW.rds")
gt_table <- readRDS("EOS Team Update NLW.rds")

gtsave(gt_table, filename = "EOS Team Update NLW.png")
gtsave(gt_table, filename = "EOS Team Update NLW.html")

webshot(
  url = "EOS Team Update NLW.html",
  file = "EOS Team Update NLW.png",
  vwidth = 1030,     # render bigger
  vheight = 976,
  zoom = 0.5         # then shrink down
)

file_name <- "EOS Team Update NLW"
cat(sprintf("✅ Saved PNG at 515x488: %s\n", file_name))
# Optional: also save as PNG (requires webshot or chromote)

gtsave(gt_table, filename = "EOS Team Update NLW.png")
gtsave(gt_table, filename = "EOS Team Update NLW.html")



