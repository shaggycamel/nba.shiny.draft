# Season variables
cur_season <- "2026-27"
prev_season <- "2025-26"
# cur_date <-

# Save all to data folder
usethis::use_data(
  cur_season,
  prev_season,
  overwrite = TRUE
)
