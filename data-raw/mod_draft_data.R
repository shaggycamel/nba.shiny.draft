# Box score query for previous season.
# Leave un-aggregated for stat-variance calc which happens in R
query <- glue_sql(
  "SELECT COALESCE(player_name, player_id::STRING) AS player_name, game_id, game_date,
    min, fgm, fga, fg3_m, ftm, fta, pts, reb, ast, stl, blk, tov, dd2, td3
  FROM nba.nba_player_box_score_vw
  WHERE season = {prev_season}
    AND season_type = 'Regular Season'",
  .con = db_con
)

# Previous season box score
df_nba_player_box_score_prev_season <- db_get_query(db_con, query)

# Players arranged by desc(sum(min))
# Vector to be used in selectInput
active_players <- df_nba_player_box_score_prev_season |>
  summarise(min = sum(min, na.rm = TRUE), .by = player_name) |>
  arrange(desc(min)) |>
  pull(player_name)

# Write objects to data folder
usethis::use_data(
  df_nba_player_box_score_prev_season,
  active_players,
  overwrite = TRUE
)
