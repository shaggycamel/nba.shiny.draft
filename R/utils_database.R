# Read only cockroach database connection\
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
db_con <- function() {
  # Cockroach
  dbConnect(
    drv = Postgres(),
    user = "kobe_public",
    password = "kobe123-69(.)(.)",
    host = "nba-data-mgmt-9184.8nj.gcp-europe-west1.cockroachlabs.cloud",
    port = "26257",
    dbname = "nba",
    options = "--cluster=nba-data-mgmt-9184"
  )

  # Postgres
  # dbConnect(
  #   drv = Postgres(),
  #   user = "postgres",
  #   password = "zxc123",
  #   host = "localhost",
  #   port = "5432",
  #   dbname = "nba",
  #   options = "-c search_path=nba"
  # )
}


#' @importFrom DBI dbGetQuery
#' @importFrom tibble as_tibble
#' @importFrom purrr keep map
#' @importFrom dplyr mutate across
db_get_query <- function(connection, query) {
  connection |>
    dbGetQuery(query) |>
    as_tibble() |>
    (\(df) {
      c_typ <- keep(map(df, class), \(x) "integer64" %in% x)
      mutate(df, across(names(c_typ), \(x) as.integer(x)))
    })()
}


#' @importFrom DBI dbAppendTable Id
db_append_record <- function(connection, df, schma, tbl) {
  dbAppendTable(connection, Id(schema = schma, table = tbl), df)
}


#' @importFrom DBI dbBegin dbExecute dbCommit
db_delete_record <- function(connection, query) {
  dbBegin(connection)
  dbExecute(connection, query)
  dbCommit(connection)
}
