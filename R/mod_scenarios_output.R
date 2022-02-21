#' scenarios_output UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_scenarios_output_ui <- function(id) {
  ns <- NS(id)
  tagList(
    risk_panel_ui(ns("risk_link"), ns = ns),
    benefit_panel_ui(ns("benefit_link"), ns = ns),
    vac_panel_ui(ns("vac_link"), ns = ns)
  )
}

#' scenarios_output Server Functions
#'
#' @noRd
mod_scenarios_output_server <- function(id, dist_args) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    data_test  <- reactive_dist(dist_args)
    data_test1 <- reactive_dist(const_testing(dist_args, 1, 1))
    data_test0 <- reactive_dist(const_testing(dist_args))


    # Infection risk (# of active cases)
    risk_panel_server("risk_link")
    data_risk <- reactive(prep_risk(
      data_test = data_test(),
      data_no_test = data_test0(),
      n = dist_args$n()
    ), label = "prep_risk()")
    output$risk_plot <- renderHighchart(plot_risk(data_risk()))
    output$risk_expl <- renderUI(explain_risk(data_risk(), n = dist_args$n()))

    # Testing benefit (reduction in active cases)
    benefit_panel_server("benefit_link")
    data_benefit <- reactive(prep_benefit(
      data_test = data_test(),
      data_no_test = data_test0(),
      n = dist_args$n()
    ), label = "prep_benefit()")
    output$benefit_plot <- renderHighchart(plot_benefit(data_benefit()))
    output$benefit_expl <- renderUI(explain_benefit(data_benefit()))

    # Vaccination comparison
    vac_panel_server("vac_link")
    data_vac <- reactive(prep_vac(
      data_test0 = data_test0(),
      data_test1 = data_test1()
    ), label = "prep_vac()")
    output$vac_plot <- renderHighchart(plot_vac(data_vac()))
    output$vac_expl <- renderUI(explain_vac(data_vac()))
  })
}
