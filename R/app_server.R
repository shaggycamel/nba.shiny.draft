#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
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
  mod_draft_server("draft_1", carry_thru)
}
