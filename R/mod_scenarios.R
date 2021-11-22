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
      column(width = 8,
             h3("Infection Risk (Active COVID-19 Cases)",
                ct_info_ui(ns("risk_plot_info"))),
             plotOutput(ns("risk_plot")),
             h3("Testing Benefit (Infection Risk Reduction)",
                ct_info_ui(ns("benefit_plot_info"))),
             plotOutput(ns("benefit_plot"))),
      column(width = 4, scenarios_input(id))
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
    # Risk plot
    output$risk_plot <- renderPlot(shinipsum::random_ggplot("bar"))
    # Risk help
    observeEvent(input$risk_plot_info, ct_info_server(p(HTML(
      "Infection risk is controlled by the number of people in an organization",
      " who have COVID-19. We call these people <b>active cases</b>.",
      " For an active case to contribute to organizational risk,",
      " they must be in physical proximity with others in the organization.",
      " Some cases will develop symptoms and seek",
      " testing themselves; assuming a correct test result,",
      " these cases will not contribute to organizational infection risk.",
      " Other cases will only develop mild symptoms, or none at all.",
      " Regular testing allows organizations to detect and isolate",
      " the people who may not know they are spreading COVID-19.",
      "<br><br>",
      "This plot shows three levels of risk for the scenario given.",
      " The first is the expected number of active cases in the organization;",
      " this is the organizational risk if no one isolated.",
      " The second is the expected number of cases without symptoms;",
      " this is the organizational risk if symptomatic people isolated",
      " (possibly imperfectly). The last is the expected number of cases",
      " that remain undetected after implementing the chosen testing strategy;",
      " even with testing, there will be a certain amount of unavoidable risk."
      ))))
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
