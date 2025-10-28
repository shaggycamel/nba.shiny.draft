#' login_modal Server Functions
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS
mod_login_modal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values for selections and met_parameters boolean
    fty_parameters_met <- reactiveVal(FALSE)
    selected <- reactiveValues()

    # Main code to assign values to reactive and close modal when complete
    observe({
      if (input$fty_league_select != "") {
        selected$league_name <- input$fty_league_select
        selected$league_id <- ls_fty_lookup[["name_to_id"]][[selected$league_name]]
        selected$platform <- ls_fty_lookup[["id_to_platform"]][[as.character(selected$league_id)]]
        fty_parameters_met(TRUE)
        removeModal()
        output$login_messages <- NULL
      } else {
        output$login_messages <- renderText("Select a league...")
      }
    }) |>
      bindEvent(input$fty_dash_init, ignoreInit = TRUE)

    # Gotta fill the form at least once!
    observe({
      if (!fty_parameters_met()) {
        output$login_messages <- renderText("You gotta go fill the form at least once!")
      } else {
        removeModal()
        output$login_messages <- NULL
      }
    }) |>
      bindEvent(input$fty_abort)

    # Modal UI structure.
    # # This needs to be defined in the server component.
    showModal(
      modalDialog(
        tags$head(tags$style(HTML(".selectize-dropdown-content{min-width: 100%; box-sizing: border-box;}"))),
        selectizeInput(
          ns("fty_league_select"),
          label = NULL,
          choices = unique(df_fty_base$league_name),
          options = list(
            placeholder = "Select Fantasy League",
            onInitialize = I("function(){this.setValue('');}")
          ),
          width = "100%"
        ),
        span(textOutput(ns("login_messages")), style = "color:red"),
        footer = tagList(
          actionButton(
            ns("fty_abort"),
            label = NULL,
            icon = icon("square-xmark"),
            style = "color:#FFF; background-color:#CD3333; border-color:#2E6DA4"
          ),
          actionButton(
            ns("fty_dash_init"),
            "Kobeee!",
            style = "color:#FFF; background-color:#337AB7; border-color:#2E6DA4"
          )
        ),
        size = "m"
      )
    )

    # Return reactive list containing selected elements
    selected
  })
}

## To be copied in the server
# mod_login_modal_server("login_modal_1")
