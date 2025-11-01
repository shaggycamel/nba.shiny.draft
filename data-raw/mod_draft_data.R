# Box score query for previous season.
# Leave un-aggregated for stat-variance calc which happens in R
query <- glue_sql(
  "SELECT COALESCE(player_name, player_id::STRING) AS player_name, game_id, game_date,
    min, fgm, fga, fg3_m, ftm, fta, pts, reb, ast, stl, blk, tov, dd2, td3
  FROM nba.nba_player_box_score_vw
  WHERE season = {prev_season}
    AND season_type = 'Regular Season'
    AND (
      player_name IS NOT NULL 
      AND player_id IS NOT NULL
    )",
  .con = db_con
)

# Zscore function
calc_z_pcts <- function(df, class) {
  numerator <- sym(str_replace(class, "_", "m_"))
  denominator <- sym(str_replace(class, "_", "a_"))

  df |>
    mutate(
      !!str_c(class, "_pct") := coalesce(!!numerator / !!denominator, 0),
      !!str_c(class, "_impact") := (!!sym(str_c(class, "_pct")) -
        (sum(!!numerator, na.rm = TRUE) / sum(!!denominator, na.rm = TRUE))) *
        !!denominator,
      !!str_c(class, "_z") := (!!sym(str_c(class, "_impact")) - mean(!!sym(str_c(class, "_impact")), na.rm = TRUE)) /
        sd(!!sym(str_c(class, "_impact")), na.rm = TRUE),
    ) |>
    select(-ends_with("impact"))
}

# Previous season box score
df_nba_player_box_score_prev_season <- db_get_query(db_con, query)
agg_cols <- str_subset(colnames(df_nba_player_box_score_prev_season), "player_name|game_id|game_date", negate = TRUE)


df_nba_player_box_score_prev_season <- df_nba_player_box_score_prev_season |>

  # Calculate covariance, mean and sum for all cats
  summarise(
    across(all_of(agg_cols), \(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE), .names = "{.col}_cov"),
    across(all_of(agg_cols), \(x) mean(x, na.rm = TRUE), .names = "{.col}_mean"),
    across(all_of(agg_cols), \(x) sum(x, na.rm = TRUE), .names = "{.col}_sum"),
    .by = c(player_name)
  ) |>

  # scale by minutes
  mutate(
    across(
      all_of(str_subset(str_c(agg_cols, "_mean"), "^min", negate = TRUE)),
      \(x) x / min_mean,
      .names = "{.col}_scaled"
    ),
    across(
      all_of(str_subset(str_c(agg_cols, "_sum"), "^min", negate = TRUE)),
      \(x) x / min_sum,
      .names = "{.col}_scaled"
    ),
  ) |>

  # calcualte mean and sum z-scores for field goals and free throws
  calc_z_pcts("fg_mean") |>
  calc_z_pcts("fg_sum") |>
  calc_z_pcts("fg_mean_scaled") |>
  calc_z_pcts("fg_sum_scaled") |>
  calc_z_pcts("ft_mean") |>
  calc_z_pcts("ft_sum") |>
  calc_z_pcts("ft_mean_scaled") |>
  calc_z_pcts("ft_sum_scaled") |>

  # Rank everyone on everything
  mutate(
    across(
      matches("_mean$|_sum$|_scaled$|_pct$|_z$"),
      \(x) if (str_detect(cur_column(), "tov")) dense_rank(x) else dense_rank(desc(x)),
      .names = "{.col}_rank"
    )
  )


# Players arranged by desc(sum(min)): Vector to be used in selectInput
active_players <- arrange(df_nba_player_box_score_prev_season, desc(min_sum))$player_name

# Write objects to data folder
usethis::use_data(
  df_nba_player_box_score_prev_season,
  active_players,
  overwrite = TRUE
)
