# Fantasy base
df_fty_base <- db_get_query(
  db_con,
  glue_sql(
    "SELECT * FROM fty.league WHERE season = {cur_season} ORDER BY league_name",
    .con = db_con
  )
)

df_fty_cats <- db_get_query(
  db_con,
  glue_sql(
    "SELECT * FROM fty.fty_categories_vw WHERE (season = {cur_season} OR league_id IS NULL)",
    .con = db_con
  )
)

ls_fty_lookup <- list(
  "name_to_id" = as.list(deframe(select(df_fty_base, league_name, league_id))),
  "id_to_name" = as.list(deframe(select(df_fty_base, league_id, league_name))),
  "id_to_platform" = as.list(deframe(select(df_fty_base, league_id, platform)))
)

usethis::use_data(
  df_fty_base,
  df_fty_cats,
  ls_fty_lookup,
  overwrite = TRUE
)

View(df_fty_cats)
