# db_connect()/db_pool() open connections to the NBA database. Tests can't
# assume a Postgres/Cockroach instance is running, so they attempt the
# connection and skip when it isn't reachable.

test_that("db_connect opens a valid connection when the database is up", {
  con <- tryCatch(db_connect(), error = function(e) NULL)
  skip_if(is.null(con), "no local NBA database available")

  on.exit(DBI::dbDisconnect(con), add = TRUE)

  expect_true(DBI::dbIsValid(con))
})

test_that("db_pool serves queries when the database is up", {
  con <- tryCatch(db_pool(), error = function(e) NULL)
  skip_if(is.null(con), "no local NBA database available")

  on.exit(poolClose(con), add = TRUE)

  expect_equal(DBI::dbGetQuery(con, "SELECT 1 AS one")$one, 1L)
})

test_that("db_config falls back to env vars when no ini file exists", {
  withr::local_envvar(
    COCKROACH_READ_USER = "u",
    COCKROACH_READ_PASSWORD = "p",
    COCKROACH_READ_HOST = "h",
    COCKROACH_READ_PORT = "1234",
    COCKROACH_READ_DBNAME = "db"
  )

  cfg <- db_config("cockroach-read", file = tempfile())

  expect_equal(cfg$user, "u")
  expect_equal(cfg$host, "h")
  expect_equal(cfg$dbname, "db")
})
