#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib page_navbar nav_spacer nav_panel nav_menu nav_item bs_theme
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    page_navbar(
      id = "title_container",
      window_title = "NBA Fantasy",
      title = uiOutput("navbar_title"),
      # put blank login page somewhere
      nav_spacer(),
      nav_panel(NULL, mod_draft_ui("draft_1")),
      nav_item(actionButton(
        "fty_league_competitor_switch",
        "League",
        icon = icon("right-from-bracket"),
        width = "150px"
      )),
      theme = bs_theme(
        version = 5,
        preset = "litera",
        primary = "#133DEF"
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "nba.shiny.draft"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
