# Read only cockroach database connection
#' @importFrom ini read.ini
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
db_con <- function(source = Sys.getenv("NBA_DB_SOURCE", "cockroach")) {
  source <- match.arg(source, c("cockroach", "postgres"))
  cfg <- db_config(source)

  args <- list(
    drv = Postgres(),
    user = cfg$user,
    password = cfg$password,
    host = cfg$host,
    port = cfg$port,
    dbname = cfg$dbname
  )
  if (!is.null(cfg$options)) {
    args$options <- cfg$options
  }

  do.call(dbConnect, args)
}


#' @noRd
db_config <- function(source) {
  if (file.exists("credentials.ini")) {
    cfg <- ini::read.ini("credentials.ini")[[source]]
    if (is.null(cfg)) {
      stop("No [", source, "] section in credentials.ini", call. = FALSE)
    }
    return(cfg)
  }

  prefix <- toupper(source)
  get_var <- function(key) {
    val <- Sys.getenv(paste0(prefix, "_", key))
    if (identical(val, "")) NULL else val
  }

  cfg <- list(
    user = get_var("USER"),
    password = get_var("PASSWORD"),
    host = get_var("HOST"),
    port = get_var("PORT"),
    dbname = get_var("DBNAME"),
    options = get_var("OPTIONS")
  )

  required <- c("user", "password", "host", "port", "dbname")
  missing <- required[vapply(cfg[required], is.null, logical(1))]
  if (length(missing) > 0) {
    stop(
      "Missing env vars for source '",
      source,
      "': ",
      paste0(prefix, "_", toupper(missing), collapse = ", "),
      call. = FALSE
    )
  }

  cfg
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
