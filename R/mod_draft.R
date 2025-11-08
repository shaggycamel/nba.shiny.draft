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
          max = ceiling(filter_quantiles[["min_mean"]][["100%"]]),
          value = ceiling(filter_quantiles[["min_mean"]][["75%"]]),
          ticks = FALSE
        ),
        sliderInput(
          ns("draft_top_n"),
          "Top N Players",
          min = 10,
          max = 30,
          value = 30,
          ticks = FALSE
        ),
        sliderInput(
          ns("draft_cov_filter"),
          "Variance Coefficient",
          step = 0.01,
          min = 0,
          max = 1,
          value = round(filter_quantiles[["min_cov"]][["25%"]], 2) + 0.01,
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
#' @importFrom dplyr slice_max if_else n
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom tibble tibble
#' @importFrom stringr str_replace_all str_remove str_remove_all str_c str_detect
#' @importFrom shinycssloaders showPageSpinner hidePageSpinner
#' @importFrom ggplot2 ggplot aes geom_col guides guide_legend theme_bw labs
#' @importFrom plotly renderPlotly ggplotly config layout
#' @importFrom rlang sym
mod_draft_server <- function(id, carry_thru, db_con) {
  moduleServer(id, function(input, output, session) {
    #
    # Init and variables -----------------------------------------------------
    ns <- session$ns
    player_draft_stream <- reactiveVal()

    # Update UI --------------------------------------------------------------

    # Update select inputs
    observe({
      req(carry_thru()$fty_parameters_met())

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
    }) |>
      bindEvent(carry_thru()$fty_parameters_met())

    # category selection
    observe({
      req(carry_thru()$fty_parameters_met())
      cur_sel <- if (input$draft_stat == "") "min" else input$draft_stat

      updateSelectInput(
        session,
        "draft_stat",
        selected = if (input$draft_scale_minutes & cur_sel == "min") "pts" else cur_sel,
        choices = relevant_cats() # Defined further down script
      )
    }) |>
      bindEvent(player_draft_stream(), input$draft_scale_minutes)

    # Minute filter
    observe({
      req(carry_thru()$fty_parameters_met())
      rng <- if (input$draft_tot_avg_toggle) filter_quantiles[["min_mean"]] else filter_quantiles[["min_sum"]]
      updateSliderInput(session, "draft_min_filter", value = rng[["75%"]], max = rng[["100%"]])
    }) |>
      bindEvent(input$draft_tot_avg_toggle, ignoreInit = TRUE)

    # Variance Coefficient filter
    observe({
      req(carry_thru()$fty_parameters_met())
      rng <- filter_quantiles[[str_c(input$draft_stat, "_cov")]]
      updateSliderInput(session, "draft_cov_filter", value = rng[["25%"]])
    }) |>
      bindEvent(input$draft_stat, ignoreInit = TRUE)

    # Track draft selection in the database
    observe({
      req(carry_thru()$fty_parameters_met())

      # Append record to database
      if (length(unique(player_draft_stream()$player_name)) < length(input$draft_player_log)) {
        #
        showPageSpinner(type = 6, caption = "Writing to database...")
        nm <- tibble(
          league_id = carry_thru()$selected$league_id,
          player_name = setdiff(input$draft_player_log, player_draft_stream()$player_name)
        )

        db_append_record(db_con, nm, "util", "draft_player_log")
        hidePageSpinner()
        #
        # Delete record from database
      } else if (length(unique(player_draft_stream()$player_name)) > length(input$draft_player_log)) {
        #
        showPageSpinner(type = 6, caption = "Deleting from database...")
        nm <- setdiff(player_draft_stream()$player_name, input$draft_player_log) |>
          paste(collapse = "', '")

        db_delete_record(
          db_con,
          glue_sql("DELETE FROM util.draft_player_log WHERE player_name IN ({nm})", .con = db_con)
        )
        hidePageSpinner()
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

    # Relevant categories
    relevant_cats <- reactive(cat_specs(
      carry_thru()$selected$league_id,
      incl_nba_cat = c("min", "fg_z", "ft_z", "tov_rt"),
      excl_nba_cat = c("fg_pct", "ft_pct", "tov", if (input$draft_scale_minutes) "min" else NULL)
    ))

    # Main data prep
    df <- reactive({
      #
      pattern_extract <- str_c(handle_cols()$operation, handle_cols()$scaled)

      df_nba_player_box_score_prev_season |>
        filter(!player_name %in% input$draft_player_log) |>
        filter(!!sym(handle_cols()$min_col) >= input$draft_min_filter) |>
        select(
          player_name,
          all_of(str_c(relevant_cats(), pattern_extract)),
          all_of(str_c(relevant_cats(), str_c(pattern_extract, "_rank"))),
          all_of(str_c(relevant_cats(), "_cov"))
        ) |>
        pivot_longer(-player_name, names_to = "cat") |>
        mutate(
          class = str_remove(cat, paste0(unlist(relevant_cats(), use.names = FALSE), collapse = "|")),
          class = if_else(str_detect(class, "_rank"), "rank", str_remove(class, "_")),
          cat = str_remove(cat, "_[^_]+$"),
          cat = str_remove_all(cat, "_sum|_mean|_scaled")
        ) |>
        pivot_wider(names_from = class, values_from = value) |>
        filter(!(cat == handle_cols()$stat_cat & cov > as.numeric(input$draft_cov_filter))) |>
        slice_max(rank, n = as.numeric(input$draft_top_n), by = cat) |>
        mutate(
          top_cats = paste(sort(cat), collapse = ", "),
          top_cats_count = n(),
          .by = player_name
        ) |>
        filter(cat == handle_cols()$stat_cat)
    })

    # Plot -------------------------------------------------------------------

    output$draft_stat_plot <- renderPlotly({
      req(carry_thru()$fty_parameters_met())

      pattern_extract <- sym(str_remove(str_c(handle_cols()$operation, handle_cols()$scaled), "_"))

      plt <- df() |>
        ggplot(aes(
          x = !!pattern_extract,
          y = if (handle_cols()$stat_cat == "tov_rt") {
            reorder(player_name, -!!pattern_extract)
          } else {
            reorder(player_name, !!pattern_extract)
          },
          fill = ordered(top_cats_count),
          text = top_cats
        )) +
        geom_col() +
        guides(fill = guide_legend(title = "Other Category Count", reverse = TRUE)) +
        labs(
          # fmt: skip
          title = str_c(
            "Previous Seasion (", prev_season,"): ",
            ifelse(input$draft_tot_avg_toggle, "Average", "Total"), " ",
            names(keep(relevant_cats(), \(x) x == handle_cols()$stat_cat)),
            ifelse(handle_cols()$scaled == "", "", " Scaled")
          ),
          x = NULL,
          y = NULL
        ) +
        theme_bw()

      # plotly
      ggplotly(plt, tooltip = "text") |>
        layout(legend = list(x = 100, y = 0.5)) |>
        reverse_legend_labels() |>
        config(displayModeBar = FALSE)
    })
  })
}

## To be copied in the UI
# mod_draft_ui("draft_1")

## To be copied in the server
# mod_draft_server("draft_1")

# library(shiny)
# library(bslib)
# library(shinyWidgets)
# library(plotly)
# library(stringr)
# library(purrr)
# library(tibble)
# library(DBI)
# library(RPostgres)
# library(shinycssloaders)
# library(glue)
# library(dplyr)
# library(tidyr)
# source(here::here("R", "utils_database.R"))
# source(here::here("R", "utils_helpers.R"))
# load("data/prev_season.rda")
# load("data/active_players.rda")
# load("data/df_fty_cats.rda")
# load("data/filter_quantiles.rda")
# load("data/df_nba_player_box_score_prev_season.rda")

# ui <- page_fluid(
#   mod_draft_ui("draft_1")
# )

# server <- function(input, output, session) {
#   carry_thru <- reactiveVal(list(
#     fty_parameters_met = reactiveVal(TRUE),
#     selected = reactiveValues(league_id = 95537)
#   ))

#   showPageSpinner(type = 6, caption = "Creating connection to database...")
#   db_con <- db_con()
#   mod_draft_server("draft_1", carry_thru, db_con)
#   hidePageSpinner()
# }

# shinyApp(ui, server)
