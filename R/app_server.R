#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #
  # ------- Database connection and init page spinner
  showPageSpinner(type = 6, caption = "Creating connection to database...")
  db_con <- db_con()

  # ------- Base reactive
  carry_thru <- reactiveVal()

  #------- Login modal
  observe(carry_thru(mod_login_modal_server("login_modal_1"))) |>
    bindEvent(input$fty_league_competitor_switch, ignoreNULL = FALSE)

  # update dashboard title with selected league
  output$navbar_title <- renderUI({
    req(carry_thru()$selected$league_name)
    span(carry_thru()$selected$league_name)
  })

  #------- Draft Page
  observe(mod_draft_server("draft_1", carry_thru, db_con)) |>
    bindEvent(carry_thru()$fty_parameters_met())

  #------- Hide page spinner
  hidePageSpinner()
}
