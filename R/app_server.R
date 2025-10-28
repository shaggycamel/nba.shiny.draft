#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  #
  #------- Login modal
  selected <- mod_login_modal_server("login_modal_1")
  observe(output$navbar_title <- renderUI(span(selected$league_name))) |>
    bindEvent(selected)

  # Select different competitor
  observe(selected <- mod_login_modal_server("login_modal_1")) |>
    bindEvent(input$fty_league_competitor_switch, ignoreInit = TRUE)
}
