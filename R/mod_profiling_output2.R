#' profiling_output2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_profiling_output2_ui <- function(id){
  ns <- NS(id)
  tagList(
    highchartOutput(ns("plot"))
  )
}

#' profiling_output2 Server Functions
#'
#' @noRd
mod_profiling_output2_server <- function(id, inputs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    x <- reactive(seq_profile(inputs$n$org()))
    y <- reactive(profile_n2(x()))

    output$plot <- renderHighchart(
      hchart(data.table(x = x(), y = y()), "line", hcaes(x = x, y = y))
    )
  })
}

profile_n2 <- function(x, y_lbl = NULL) {
  d <- calc_dist()
  p <- undetected(d)

  p * x
}

## To be copied in the UI
# mod_profiling_output2_ui("profiling_output2_ui_1")

## To be copied in the server
# mod_profiling_output2_server("profiling_output2_ui_1")
