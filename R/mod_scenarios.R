#' Scenarios UI Function
#'
#' A Shiny module containing code for COVID-19 testing scenario generation and
#' visualization
#'
#' @param id Internal parameter for {shiny}
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_scenarios_ui <- function(id) {
  ns <- NS(id)
  tags$div(
    style = "max-width: 1280px; margin: auto",
    fluidRow(
      column(width = 8, mod_scenarios_output_ui(ns("output"))),
      column(width = 4, mod_scenarios_input_ui(ns("input")))
    ),
    tags$br()
  )
}

#' Scenarios Server Functions
#'
#' A Shiny module containing code for calculating outputs of COVID-19 testing
#' scenarios
#'
#' @param id,input,output,session Internal parameters for {shiny}
#'
#' @noRd
mod_scenarios_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    dist_args <- mod_scenarios_input_server("input")

    mod_scenarios_output_server("output", dist_args = dist_args)
  })
}
