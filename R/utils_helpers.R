#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @importFrom dplyr filter bind_rows arrange distinct select
cat_specs <- function(selected_league_id, vec = FALSE, h2h = TRUE, incl_nba_cat = NULL, excl_nba_cat = NULL) {
  df_fty_cats <- filter(df_fty_cats, league_id == selected_league_id | is.na(league_id))
  obj <- if (h2h) filter(df_fty_cats, h2h_cat) else obj
  obj <- if (!is.null(incl_nba_cat)) bind_rows(obj, filter(df_fty_cats, nba_category %in% incl_nba_cat)) else obj
  obj <- if (!is.null(excl_nba_cat)) filter(obj, !nba_category %in% excl_nba_cat) else obj
  obj <- arrange(distinct(obj), display_order)
  obj <- as.list(tibble::deframe(select(obj, fmt_category, nba_category)))
  if (vec) unlist(obj, use.names = FALSE) else obj
}


reverse_legend_labels <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}
