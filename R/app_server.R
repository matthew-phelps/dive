#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Sync button observer
  observeEvent(input$sync_button, {
    showNotification("Sync functionality coming soon...", type = "message")
  })

  # Summary stats module will be called here

  # Distance view module will be called here
}
