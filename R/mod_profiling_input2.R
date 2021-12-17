#' profiling_input2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_profiling_input2_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(span(style = "margin-left: auto", select_input(ns("x_var"))),
             span(style = "width: 3rem"),
             span(style = "margin-right: auto", select_output(ns("y_var")))
    ),
    fluidRow(
      numericInput2(ns("n_org"),
                    condition = "input.x_var == 'n_org'",
                    label = "Organization Size (People)",
                    value = 1e3, value2 = c(1e2, 1e3))
    )
  )
}

#' profiling_input2 Server Functions
#'
#' @noRd
mod_profiling_input2_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # iv <- shinyvalidate::InputValidator$new()
    # iv$add_rule("n_org", shinyvalidate::sv_required())
    # iv$add_rule("n_org", sv_gte_multi(0))



    n_org <- reactive(if (input$x_var == "n_org") input$n_org2 else input$n_org)

    list(n = reactiveValues(org = n_org))
  })
}

select_input <- function(id) {
  selectInput(
    id,
    label = "Input (X-axis)",
    width = "21rem",
    choices = list(
      Organization = list(
        `Organization Size (People)` = "n_org",
        `Unvaccinated Testing (Days)` = "test_f_asymp_unvac",
        `Vaccinated Testing (Days)` = "test_f_asymp_vac",
        `% Vaccinated (in Organization)` = "vac_p_org"
      ),
      Community = list(
        `Case Rate (per 100k per day)` = "inf_r_incid",
        `% Vaccinated (in Community)` = "vac_p_comm"
      ),
      Vaccination = list(`Vaccine Efficacy (%)` = "vac_eff"),
      Testing = list(
        `Test Sensitivity (%)` = "detect_sens",
        `Test Specificity (%)` = "detect_spec",
        `% Symptomatics Tested` = "test_p_symp"
      ),
      Symptoms = list(
        `Symptomatic Period (Days)` = "inf_t_symp",
        `Pre-Symptomatic Period (Days)` = "inf_t_presymp",
        `% Symptomatic: Unvaccinated Cases` = "symp_p_inf_unvac",
        `% Symptomatic: Vaccinated Cases` = "symp_p_inf_vac",
        `% Symptomatic: Non-Cases` = "symp_p_uninf"
      )
    )
  )
}


select_output <- function(id) {
  shinyWidgets::radioGroupButtons(
    inputId = id,
    label = "Output (Y-Axis)",
    choices = list(
      Risk = "risk",
      Benefit = "benefit",
      Vaccination = "vac"
    ),
    status = "info"
  )
}

## To be copied in the UI
# mod_profiling_input2_ui("profiling_input2_ui_1")

## To be copied in the server
# mod_profiling_input2_server("profiling_input2_ui_1")
