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
#   2. a layout$shapes rectangle for the strip's gray background
#
# Repositioning the auto-generated shape directly was unreliable -- shapes
# appear to get clipped to their own subplot's plotting area once their
# coordinates extend past domain [0,1], while annotations don't (they live
# in a separate, unclipped layer, confirmed by the annotation's own
# `bgcolor` rendering fine even nudged into the margin via `xshift`).
# Annotations can't be given an arbitrary height independent of their text
# though, so a strip that spans the full row height still needs a real
# shape -- kept strictly INSIDE the domain here (flush against the panel's
# own right edge) to sidestep that clipping. The label lives in a separate,
# non-background annotation just outside it, in the margin.
# Requires labs(x = NULL, y = NULL) on the underlying ggplot so no
# axis-title annotations are mixed in with the strip annotations (which
# would break the 1:1 index-to-panel pairing).
move_facet_strips_right <- function(plotly_plot) {
  n <- length(plotly_plot$x$layout$annotations)
  strip_fill <- "rgba(217,217,217,1)"
  inset_frac <- 0.03 # fraction of each panel's own width used for the strip

  strip_shapes <- vector("list", n)

  for (i in seq_len(n)) {
    xkey <- if (i == 1) "xaxis" else paste0("xaxis", i)
    ykey <- if (i == 1) "yaxis" else paste0("yaxis", i)
    xdom <- plotly_plot$x$layout[[xkey]]$domain
    ydom <- plotly_plot$x$layout[[ykey]]$domain
    panel_w <- xdom[2] - xdom[1]

    plotly_plot$x$layout$annotations[[i]]$x <- xdom[2]
    plotly_plot$x$layout$annotations[[i]]$xanchor <- "left"
    plotly_plot$x$layout$annotations[[i]]$xshift <- 8
    plotly_plot$x$layout$annotations[[i]]$y <- mean(ydom)
    plotly_plot$x$layout$annotations[[i]]$yanchor <- "middle"
    plotly_plot$x$layout$annotations[[i]]$textangle <- 90

    strip_shapes[[i]] <- list(
      type = "rect",
      xref = "paper",
      yref = "paper",
      x0 = xdom[2] - panel_w * inset_frac,
      x1 = xdom[2],
      y0 = ydom[1],
      y1 = ydom[2],
      fillcolor = strip_fill,
      line = list(width = 0),
      layer = "above"
    )
  }

  # Drop ggplotly's own auto-generated gray strip-background shapes
  # (identified by fill color -- theme_bw()'s standard strip gray) and
  # replace with the full-height ones built above.
  shapes <- plotly_plot$x$layout$shapes
  if (!is.null(shapes) && length(shapes) > 0) {
    is_strip_bg <- vapply(
      shapes,
      function(s) {
        !is.null(s$fillcolor) && identical(s$fillcolor, strip_fill)
      },
      logical(1)
    )
    shapes <- shapes[!is_strip_bg]
  }
  plotly_plot$x$layout$shapes <- c(shapes, strip_shapes)

  current_r <- plotly_plot$x$layout$margin$r
  plotly_plot$x$layout$margin$r <- (if (is.null(current_r)) 40 else current_r) + 40
  plotly_plot
}
