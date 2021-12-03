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
mod_scenarios_ui <- function(id){
  ns <- NS(id)
  tags$div(
    style = "max-width: 1280px; margin: auto",
    p("Step through the scenario builder to see your organization's",
      "risk profile and testing strategies"
      ),
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
mod_scenarios_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    dist_args <- mod_scenarios_input_server("input")

    mod_scenarios_output_server("output", dist_args = dist_args)

    # Benefit plot
    output$benefit_plot <- renderPlot(shinipsum::random_ggplot("bar"))
    # Benefit help
    observeEvent(input$benefit_plot_info, ct_info_server(p(HTML(
      "Regular testing provides its benefits by reducing the number of people",
      " spreading COVID-19 in an organization. That reduction can be assessed",
      " by comparing the infection risk for a testing strategy to either",
      " the total infection risk (with no isolation) or only the risk from",
      " cases that do not seek testing themselves. We are comparing",
      " the third bar in the <b>Infection Risk</b> chart",
      " with the first and second bars.",
      "<br><br>",
      "This plot shows the differences we just described. The first bar",
      " shows the difference with respect to total infection risk; the second",
      " shows the difference with respect to the risk from non-test-seeking",
      " cases. This can be viewed as an absolute number or a percentage. The",
      " absolute number is most relevant to infection risk, because risk rises",
      " with each person spreading COVID-19. The percentage is most relevant",
      " when assessing the performance of a testing strategy. It is possible",
      " for a strategy to perform well but reduce absolute risk very little;",
      " likewise, a strategy can identify a large number of cases but leave",
      " an unacceptable amount undetected."
    ))))
  })
}
