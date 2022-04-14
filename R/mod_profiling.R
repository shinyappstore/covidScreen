#' profiling UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_profiling_ui <- function(id) {
  ns <- NS(id)
  id_input <- ns("input")
  tags$div(
    style = "max-width: 1280px; margin: auto",
    profiling_info(),
    mod_profiling_output_ui(ns("output"), id_input),
    mod_profiling_input_ui(ns("input")),
    tags$br()
  )
}

#' profiling Server Functions
#'
#' @noRd
mod_profiling_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    inputs <- mod_profiling_input_server("input")
    mod_profiling_output_server("output", inputs)

  })
}


profiling_info <- function() {
  tags$p(
    "Use the settings at bottom to explore particular settings in a scenario.",
    "You can choose one setting to vary over a range of values in the 'Input'",
    "field; this will show you how the selected results ('Output') change as",
    "that setting changes. Results will only update when the 'Calculate'",
    "button is clicked. To see a clearer overview of results for a specific",
    "scenario, visit the 'Scenarios' tab."
  )
}
