#' profvis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_profvis_ui <- function(id){
  profvis::profvis_ui(id)
}

#' profvis Server Functions
#'
#' @noRd
mod_profvis_server <- function(id){
  moduleServer(id, profvis::profvis_server)
}

## To be copied in the UI
# mod_profvis_ui("profvis_ui_1")

## To be copied in the server
# mod_profvis_server("profvis_ui_1")
