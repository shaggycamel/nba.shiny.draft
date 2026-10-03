# Resolve credentials for a section: ini file if present, else env vars
db_config <- function(
  section,
  file = Sys.getenv(
    "SPORTS_HUB_CREDENTIALS",
    "~/.config/sports-hub-credentials.ini"
  )
) {
  file <- path.expand(file)
  if (file.exists(file)) {
    creds <- ini::read.ini(file)[[section]]
    if (is.null(creds)) {
      stop("Section '", section, "' not found in ", file, call. = FALSE)
    }
    return(list(
      user = creds$user,
      password = creds$password,
      host = creds$host,
      port = creds$port,
      dbname = if (is.null(creds$database)) creds$dbname else creds$database,
      options = creds$options
    ))
  }

  prefix <- toupper(gsub("-", "_", section))
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
      "Missing env vars for section '",
      section,
      "': ",
      paste0(prefix, "_", toupper(missing), collapse = ", "),
      call. = FALSE
    )
  }

  cfg
}


# Build a single database connection from a credentials section
#' @importFrom ini read.ini
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
db_connect <- function(section) {
  cfg <- db_config(section)
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


# Build a pooled database connection from a credentials section. The pool
# validates connections on checkout and opens a fresh one if the old has gone
# stale (idle timeout, dropped TCP), so long-lived sessions stay usable.
#' @importFrom pool dbPool poolClose
db_pool <- function(section) {
  cfg <- db_config(section)
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

  do.call(dbPool, args)
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
