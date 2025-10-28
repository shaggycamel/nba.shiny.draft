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
          choice = character(0)
          # thinking off adding this to utils_*
          # choices = cat_specs(
          #   incl_nba_cat = c("min", "fg_z", "ft_z"),
          #   excl_nba_cat = c("fg_pct", "ft_pct")
          # )
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
          value = FALSE,
          onLabel = "Total",
          offLabel = "Mean",
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

#' draft Server Functions
#'
#' @noRd
#'
#' @importFrom glue glue_sql
#' @importFrom stringr str_replace_all
#' @importFrom shinycssloaders showPageSpinner hidePageSpinner
mod_draft_server <- function(id, selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    db_con <- db_con() # DELETE
    player_draft_stream <- reactiveVal()
    player_draft_stream("kick off observer")

    # Update select inputs
    observe({
      # Start loading page
      data_collection_caption <- "Processing data, one minute..."
      showPageSpinner(type = 6, caption = data_collection_caption)
      player_draft_stream(
        db_get_query(
          db_con,
          glue_sql(
            "SELECT player_name 
            FROM util.draft_player_log 
            WHERE (base_avoid IS TRUE OR league_id = {selected$league_id})",
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

      # DO THIS ---- TODO
      # updateSelectInput(session, "draft_stat", choices = cat_specs())

      # Stop loading page
      hidePageSpinner()
    }) |>
      bindEvent(player_draft_stream(), once = TRUE)

    # Track draft selection in the database
    observe({
      if (length(unique(player_draft_stream()$player_name)) < length(input$draft_player_log)) {
        nm <- tibble(
          league_id = selected$league_id,
          player_name = setdiff(input$draft_player_log, player_draft_stream()$player_name)
        )

        db_append_record(db_con, nm, "util", "draft_player_log")
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

    # NOW GET TO ACTUAL PLOT!
    # output$<- renderPlotly({})
  })
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

ui <- page_fluid(
  mod_draft_ui("draft_1")
)

server <- function(input, output, session) {
  selected <- reactiveValues(league_id = 95537)
  mod_draft_server("draft_1", selected)
}

shinyApp(ui, server)
