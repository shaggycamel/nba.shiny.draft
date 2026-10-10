# The data pipeline (data-raw/_generate_all.R) keys draft stats on
# nba.player_box_score_vw.player_name, which is resolved through the util
# player-matching layer (util.player_id_map_vw). These tests assert that
# contract. They need a live database and skip when one isn't reachable.

pipeline_db <- function() {
  tryCatch(db_connect(), error = function(e) NULL)
}

test_that("prev-season box score rows with a player_id always carry a conformed name", {
  con <- pipeline_db()
  skip_if(is.null(con), "no local NBA database available")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  bad <- DBI::dbGetQuery(
    con,
    glue::glue_sql(
      "SELECT count(*)::int AS n FROM nba.player_box_score_vw
        WHERE season = {prev_season} AND season_type = 'Regular Season'
          AND player_id IS NOT NULL AND player_name IS NULL",
      .con = con
    )
  )$n

  expect_equal(bad, 0L)
})

test_that("prev-season player_ids all resolve through util.player_id_map_vw", {
  con <- pipeline_db()
  skip_if(is.null(con), "no local NBA database available")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  res <- DBI::dbGetQuery(
    con,
    glue::glue_sql(
      "SELECT count(*)::int AS ids,
              count(*) FILTER (WHERE nm.player_key IS NULL)::int AS unmatched
         FROM (SELECT DISTINCT player_id FROM nba.player_box_score_vw
                WHERE season = {prev_season} AND season_type = 'Regular Season'
                  AND player_id IS NOT NULL) b
         LEFT JOIN util.player_id_map_vw nm ON b.player_id = nm.nba_id::FLOAT8",
      .con = con
    )
  )

  expect_gt(res$ids, 0L)
  expect_equal(res$unmatched, 0L)
})

test_that("draft log names all exist in the conformed player universe", {
  con <- pipeline_db()
  skip_if(is.null(con), "no local NBA database available")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  orphans <- DBI::dbGetQuery(
    con,
    glue::glue_sql(
      "SELECT count(DISTINCT d.player_name)::int AS n
         FROM util.draft_player_log d
         LEFT JOIN (SELECT DISTINCT player_name FROM nba.player_box_score_vw
                    WHERE season = {prev_season} AND season_type = 'Regular Season') b
           ON d.player_name = b.player_name
        WHERE b.player_name IS NULL",
      .con = con
    )
  )$n

  expect_equal(orphans, 0L)
})
