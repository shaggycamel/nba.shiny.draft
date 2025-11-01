#' draft UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput sliderInput checkboxInput absolutePanel updateSelectInput
#' @importFrom shinyWidgets switchInput dropdownButton
#' @importFrom bslib layout_sidebar sidebar card
#' @importFrom plotly plotlyOutput
#' @importFrom glue glue_sql
#' @importFrom stringr str_replace_all
mod_draft_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          ns("draft_stat"),
          "Statistic",
          choices = character(0)
        ),
        sliderInput(
          ns("draft_min_filter"),
          "Limit Minutes",
          step = 1,
          min = 0,
          max = as.numeric(0),
          value = as.numeric(0),
          ticks = FALSE
        ),
        sliderInput(
          ns("draft_top_n"),
          "Top N Players",
          min = 10,
          max = 20,
          value = 15,
          ticks = FALSE
        ),
        sliderInput(
          ns("draft_cov_filter"),
          "Variance Coefficient",
          step = 0.01,
          min = as.numeric(0),
          max = as.numeric(0),
          value = as.numeric(0),
          ticks = FALSE
        ),
        checkboxInput(
          ns("draft_scale_minutes"),
          "Scale by Minutes"
        ),
        switchInput(
          ns("draft_tot_avg_toggle"),
          value = TRUE,
          onLabel = "Mean",
          offLabel = "Total",
          size = "large"
        )
      ),
      card(
        full_screen = TRUE,
        plotlyOutput(ns("draft_stat_plot"))
      ),
      absolutePanel(
        dropdownButton(
          selectInput(
            ns("draft_player_log"),
            label = NULL,
            choices = character(0),
            multiple = TRUE,
            width = "100%"
          ),
          width = "850px"
        ),
        right = 10,
        top = 10
      ),
      border_radius = FALSE,
      fillable = TRUE,
      class = "p-0"
    )
  )
}

#' @noRd
#'
#' @importFrom glue glue_sql
#' @importFrom stringr str_replace_all
#' @importFrom shinycssloaders showPageSpinner hidePageSpinner
#' @importFrom plotly renderPlotly ggplotly
mod_draft_server <- function(id, carry_thru) {
  moduleServer(id, function(input, output, session) {
    # Init and variables -----------------------------------------------------
    ns <- session$ns
    player_draft_stream <- reactiveVal()
    player_draft_stream("kick off observer")

    # Update UI --------------------------------------------------------------

    # Update select inputs
    # HAVE A FEELING THIS ISN'T RUNNING BECAUSE player_draft_stream GETS UPDATED ON INIT...THEN THAT'S IT
    observe({
      req(carry_thru()$fty_parameters_met())

      # Start loading page
      data_collection_caption <- "Processing data, one minute..."
      showPageSpinner(type = 6, caption = data_collection_caption)

      db_con <- db_con()

      player_draft_stream(
        db_get_query(
          db_con,
          glue_sql(
            "SELECT player_name 
            FROM util.draft_player_log 
            WHERE (base_avoid IS TRUE OR league_id = {carry_thru()$selected$league_id})",
            .con = db_con
          )
        )
      )

      # main section
      updateSelectInput(
        session,
        "draft_player_log",
        choices = active_players,
        selected = player_draft_stream()$player_name
      )

      # Stop loading page
      hidePageSpinner()
    }) |>
      bindEvent(player_draft_stream())

    # category selection
    observe({
      req(carry_thru()$fty_parameters_met())
      cur_sel <- if (input$draft_stat == "") "min" else input$draft_stat

      updateSelectInput(
        session,
        "draft_stat",
        selected = if (input$draft_scale_minutes & cur_sel == "min") "pts" else cur_sel,
        choices = cat_specs(
          isolate(carry_thru()$selected$league_id),
          incl_nba_cat = c("min", "fg_z", "ft_z"),
          excl_nba_cat = c("fg_pct", "ft_pct", if (input$draft_scale_minutes) "min" else NULL)
        )
      )
    }) |>
      bindEvent(player_draft_stream(), input$draft_scale_minutes)

    observe({
      req(carry_thru()$fty_parameters_met())

      # Minute Range Filter
      min_range <- round(quantile(df_pre()[[handle_cols()$min_col]], na.rm = TRUE))
      updateSliderInput(session, "draft_min_filter", max = min_range[["100%"]], value = min_range[["75%"]])

      # Variance Coefficient filter
      cov_quantiles <- quantile(df_pre()[[handle_cols()$cov_col]], na.rm = TRUE)
      print(cov_quantiles)
      updateSliderInput(
        session,
        "draft_cov_filter",
        min = floor((cov_quantiles[["0%"]] * 10^2 / 10^2) * 0.97), # FIX
        max = round(cov_quantiles[["100%"]] * 1.03, 2), # FIX
        value = cov_quantiles[["50%"]]
      )
    }) |>
      bindEvent(input$draft_tot_avg_toggle, input$draft_stat, input$draft_scale_minutes)

    # Track draft selection in the database
    observe({
      req(carry_thru()$fty_parameters_met())

      # Append record to database
      if (length(unique(player_draft_stream()$player_name)) < length(input$draft_player_log)) {
        nm <- tibble(
          league_id = carry_thru()$selected$league_id,
          player_name = setdiff(input$draft_player_log, player_draft_stream()$player_name)
        )

        db_append_record(db_con, nm, "util", "draft_player_log")

        # Delete record from database
      } else if (length(unique(player_draft_stream()$player_name)) > length(input$draft_player_log)) {
        nm <- setdiff(player_draft_stream()$player_name, input$draft_player_log) |>
          paste(collapse = "', '")

        db_delete_record(
          db_con,
          glue_sql("DELETE FROM util.draft_player_log WHERE player_name IN ({nm})", .con = db_con)
        )
      }

      player_draft_stream(tibble(player_name = input$draft_player_log))
    }) |>
      bindEvent(input$draft_player_log, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Data prep --------------------------------------------------------------

    # Reactive list of cols that will be used
    handle_cols <- reactive(list(
      stat_cat = input$draft_stat,
      cov_col = str_c(input$draft_stat, "_cov"),
      min_col = str_c("min", if (input$draft_tot_avg_toggle) "_mean" else "_sum"),
      operation = if (input$draft_tot_avg_toggle) "_mean" else "_sum",
      scaled = if (input$draft_scale_minutes) "_scaled" else ""
    ))

    # Main data pre: User defined filters | Scale by minutes (if selected)
    df_pre <- reactive({
      req(caarry_thru()$fty_parameters_met())

      df_nba_player_box_score_prev_season |>
        filter(!!sym(handle_cols()$min_col) >= input$draft_min_filter) |>
        filter(!player_name %in% input$draft_player_log)
    })

    # Main data -- UPTO HERE.
    # HOW TO SELECT RELEVANT COLS
    # NEST BY RELEVANT CATS to SLICE BY RANK N
    # COMBINE BACK to calculate top cats by player
    # create tops cats col and count
    #Filter to just what is necessarsy
    # Check covariance filter is working as expected
    df <- reactive({
      df_inr <- df_pre()
      # select(matches(
      #   cat_specs(
      #     isolate(carry_thru()$selected$league_id),
      #     vec = TRUE,
      #     incl_nba_cat = c("min", "fg_z", "ft_z"),
      #     excl_nba_cat = c("fg_pct", "ft_pct", if (input$draft_scale_minutes) "min" else NULL)
      #   ) |>
      #     paste0(collapse = "|")
      # ))
      # select(matches(if (input$draft_tot_avg_toggle) "mean" else "sum"))

      max_min_players <- slice_max(df_inr, order_by = !!sym(handle_cols()$min_col), prop = 0.35)$player_name

      # mutate(top_cats = if_else(rank <= input$draft_top_n, stat, NA)) |>
      #   mutate(top_cats = na_if(paste(na.omit(top_cats), collapse = ", "), ""), .by = player_name) |>
      #   mutate(top_cat_count = str_count(top_cats, ",") + 1)
    })
  })

  # Plot -------------------------------------------------------------------

  #   output$draft_stat_plot <- renderPlotly({
  #     req(carry_thru()$fty_parameters_met())

  #     plt <- df() |>
  #       ggplot(aes(
  #         x = value,
  #         y = if (input$draft_stat == "tov") {
  #           reorder(player_name, -value)
  #         } else {
  #           reorder(player_name, value)
  #         },
  #         fill = ordered(top_cat_count),
  #         text = top_cats
  #       )) +
  #       geom_col() +
  #       guides(fill = guide_legend(title = "Other Category Count", reverse = TRUE)) +
  #       labs(
  #         # fmt: skip
  #         title = str_c(
  #           "Previous Seasion (", prev_season,"): ",
  #           ifelse(input$draft_tot_avg_toggle, "Total", "Average"), " ",
  #           names(keep(cat_specs(h2h=FALSE), \(x) x == input$draft_stat)), ifelse(input$draft_scale_minutes, " Scaled", "")
  #         ),
  #         x = NULL,
  #         y = NULL
  #       ) +
  #       theme_bw()

  #     # plotly
  #     ggplotly(plt, tooltip = "text") |>
  #       layout(legend = list(x = 100, y = 0.5)) |>
  #       reverse_legend_labels() |>
  #       config(displayModeBar = FALSE)
  # })
}

## To be copied in the UI
# mod_draft_ui("draft_1")

## To be copied in the server
# mod_draft_server("draft_1")

library(shiny)
library(bslib)
library(shinyWidgets)
library(plotly)
library(stringr)
library(purrr)
library(tibble)
library(DBI)
library(RPostgres)
library(shinycssloaders)
library(glue)
source(here::here("R", "utils_database.R"))
source(here::here("R", "utils_helpers.R"))

ui <- page_fluid(
  mod_draft_ui("draft_1")
)

server <- function(input, output, session) {
  carry_thru <- reactiveVal(list(
    fty_parameters_met = reactiveVal(TRUE),
    selected = reactiveValues(league_id = 95537)
  ))
  mod_draft_server("draft_1", carry_thru)
}

shinyApp(ui, server)
