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

# ggplotly() renders facet strip labels as ordinary layout annotations
# (xref/yref = "paper", positioned top-center of each panel) rather than
# real facet strips, so ggplot2's strip.position has no effect once piped
# through ggplotly(). This repositions those same annotations to the right
# edge of each panel, vertically centered and rotated, to fake the
# equivalent of strip.position = "right". Requires labs(x = NULL, y = NULL)
# on the underlying ggplot so no axis-title annotations are mixed in with
# the strip annotations (which would break the 1:1 index-to-panel pairing).
move_facet_strips_right <- function(plotly_plot) {
  n <- length(plotly_plot$x$layout$annotations)
  for (i in seq_len(n)) {
    xkey <- if (i == 1) "xaxis" else paste0("xaxis", i)
    ykey <- if (i == 1) "yaxis" else paste0("yaxis", i)
    xdom <- plotly_plot$x$layout[[xkey]]$domain
    ydom <- plotly_plot$x$layout[[ykey]]$domain
    plotly_plot$x$layout$annotations[[i]]$x <- xdom[2]
    plotly_plot$x$layout$annotations[[i]]$xanchor <- "left"
    plotly_plot$x$layout$annotations[[i]]$xshift <- 5
    plotly_plot$x$layout$annotations[[i]]$y <- mean(ydom)
    plotly_plot$x$layout$annotations[[i]]$yanchor <- "middle"
    plotly_plot$x$layout$annotations[[i]]$textangle <- 90
  }
  current_r <- plotly_plot$x$layout$margin$r
  plotly_plot$x$layout$margin$r <- (if (is.null(current_r)) 40 else current_r) + 40
  plotly_plot
}
