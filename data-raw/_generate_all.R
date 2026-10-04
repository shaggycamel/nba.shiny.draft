library(DBI)
library(RPostgres)
library(here)
library(purrr)
library(glue)
library(tibble)
library(dplyr)
library(stringr)
library(tidyr)

# database functions
source(here("R", "utils_database.R"))

# generate data files
files <- list.files(here("data-raw"), pattern = "^[^_]")

# Generate Data ----------------------------------------------------------

# db_con <- db_connect("postgres")
db_con <- db_connect("cockroach-read")

walk(files, \(file) {
  source(here("data-raw", file))
})

DBI::dbDisconnect(db_con)
