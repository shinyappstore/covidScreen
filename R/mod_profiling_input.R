#' profiling_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_profiling_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(span(style = "margin-left: auto", select_input(ns("x_var"))),
             span(style = "width: 3rem"),
             span(style = "margin-right: auto", select_output(ns("y_var")))
    ),
    # Organization
    column(width = 4),
    # Community
    column(width = 4),
    # Advanced
    column(width = 4)
  )
}

#' profiling_input Server Functions
#'
#' @noRd
mod_profiling_input_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
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

  id_quo <- rlang::enquo(id)

  if (rlang::quo_is_call(id_quo)) {
    ns     <- rlang::call_fn(id_quo)
    id_chr <- eval(rlang::call_args(id_quo)[[1]])
  } else {
    ns     <- NS(NULL)
    id_chr <- eval(!!id_quo)
  }

  id_radio <- paste0(id_chr, "_radio")

  tagList(
    shinyWidgets::radioGroupButtons(
      inputId = ns(id_radio),
      label = "Output (Y-Axis)",
      choices = list(
        Risk = "risk",
        Benefit = "benefit",
        Vaccination = "vaccination"
      ),
      status = "info"
    )
  )
}
