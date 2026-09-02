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

# ggplotly() renders each facet strip as TWO separate elements, neither of
# which respects ggplot2's strip.position:
#   1. a layout annotation for the strip text (xref/yref = "paper",
#      positioned top-center of each panel)
#   2. a layout$shapes rectangle for the strip's gray background, emitted as
#      a (transparent panel border, gray strip background) pair per panel,
#      in the same order as the panels/annotations. The gray shape's own
#      y0/y1 aren't usable for position-matching -- ggplotly gives them an
#      unrelated data-space range -- so pairing relies on this fixed order
#      instead.
# This repositions both to the right edge of each panel, vertically
# centered and rotated, to fake the equivalent of strip.position = "right".
# Requires labs(x = NULL, y = NULL) on the underlying ggplot so no
# axis-title annotations are mixed in with the strip annotations (which
# would break the 1:1 index-to-panel pairing).
move_facet_strips_right <- function(plotly_plot) {
  n <- length(plotly_plot$x$layout$annotations)
  n_shapes <- length(plotly_plot$x$layout$shapes)
  strip_w <- 0.02
  shapes_ok <- n_shapes == 2 * n

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

    if (shapes_ok) {
      strip_idx <- 2 * i
      plotly_plot$x$layout$shapes[[strip_idx]]$x0 <- xdom[2]
      plotly_plot$x$layout$shapes[[strip_idx]]$x1 <- xdom[2] + strip_w
      plotly_plot$x$layout$shapes[[strip_idx]]$y0 <- ydom[1]
      plotly_plot$x$layout$shapes[[strip_idx]]$y1 <- ydom[2]
    }
  }
  current_r <- plotly_plot$x$layout$margin$r
  plotly_plot$x$layout$margin$r <- (if (is.null(current_r)) 40 else current_r) + 40
  plotly_plot
}
