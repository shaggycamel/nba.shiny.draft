#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @importFrom dplyr mutate coalesce select
#' @importFrom tidyr ends_with
calc_z_pcts <- function(df) {
  df |>
    mutate(
      fg_pct = coalesce(fgm / fga, 0),
      ft_pct = coalesce(ftm / fta, 0),
      fg_impact = (fg_pct - (sum(fgm, na.rm = TRUE) / sum(fga, na.rm = TRUE))) * fga,
      ft_impact = (ft_pct - (sum(ftm, na.rm = TRUE) / sum(fta, na.rm = TRUE))) * fta,
      fg_z = (fg_impact - mean(fg_impact, na.rm = TRUE)) / sd(fg_impact, na.rm = TRUE),
      ft_z = (ft_impact - mean(ft_impact, na.rm = TRUE)) / sd(ft_impact, na.rm = TRUE)
    ) |>
    select(-ends_with("impact"))
}


#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @importFrom dplyr filter bind_rows arrange distinct select
cat_specs <- function(vec = FALSE, h2h = TRUE, incl_nba_cat = NULL, excl_nba_cat = NULL) {
  df_fty_cats <- filter(df_fty_cats, league_id == league_selected())
  obj <- if (h2h) filter(df_fty_cats, h2h_cat) else obj
  obj <- if (!is.null(incl_nba_cat)) bind_rows(obj, filter(df_fty_cats, nba_category %in% incl_nba_cat)) else obj
  obj <- if (!is.null(excl_nba_cat)) filter(obj, !nba_category %in% excl_nba_cat) else obj
  obj <- arrange(distinct(obj), display_order)
  obj <- as.list(tibble::deframe(select(obj, fmt_category, nba_category)))
  if (vec) unlist(obj, use.names = FALSE) else obj
}
