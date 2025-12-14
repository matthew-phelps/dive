#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    page_navbar(
      title = "dive - Activity Tracker",
      theme = bs_theme(version = 5, bootswatch = "flatly"),

      nav_panel(
        "Summary Stats",
        # Summary stats module will be added here
        h3("Summary Statistics"),
        p("Coming soon...")
      ),

      nav_panel(
        "Distance View",
        # Distance view module will be added here
        h3("Distance Trends"),
        p("Coming soon...")
      ),

      nav_spacer(),

      nav_item(
        actionButton("sync_button", "Sync Activities", icon = icon("sync"))
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
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "dive"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
