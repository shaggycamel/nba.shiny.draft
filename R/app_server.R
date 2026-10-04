#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom DBI dbDisconnect
#' @noRd
app_server <- function(input, output, session) {
  #
  # ------- Database connection and init page spinner
  showPageSpinner(type = 6, caption = "Creating connection to database...")
  db_con <- db_pool()
  session$onSessionEnded(\() poolClose(db_con))

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
  # Register once. The module gates every observer on carry_thru() internally,
  # so it does not need re-creating when a league is (re)selected. Registering
  # it from inside an event observer would add a second set of observers on the
  # same namespaced inputs, and each database write would happen once per
  # instance.
  mod_draft_server("draft_1", carry_thru, db_con)

  #------- Hide page spinner
  hidePageSpinner()
}
