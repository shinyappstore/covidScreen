#' profiling_input2 UI Function
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
    fluidRow(
      # Organization inputs
      org_profiling_column(ns("org_link"), ns = ns),
      comm_profiling_column(ns("comm_link"), ns = ns),
      advanced_profiling_column(ns("advanced_link"), ns = ns)
    )
  )
}

#' profiling_input2 Server Functions
#'
#' @noRd
mod_profiling_input_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Validate inputs
    iv <- shinyvalidate::InputValidator$new()
    # Organization size - required
    iv$add_rule("n_org-point", shinyvalidate::sv_required())
    iv$add_rule("n_org-range-1", shinyvalidate::sv_required())
    iv$add_rule("n_org-range-2", shinyvalidate::sv_required())
    # Organization size - non-negative
    iv$add_rule("n_org-point", shinyvalidate::sv_gte(0))
    iv$add_rule("n_org-range-1", shinyvalidate::sv_gte(0))
    iv$add_rule("n_org-range-2", shinyvalidate::sv_gte(0))
    # Unvaccinated test frequency - required
    iv$add_rule("test_f_asymp_unvac-point", shinyvalidate::sv_required())
    iv$add_rule("test_f_asymp_unvac-range-1", shinyvalidate::sv_required())
    iv$add_rule("test_f_asymp_unvac-range-2", shinyvalidate::sv_required())
    # Unvaccinated test frequency - non-negative
    iv$add_rule("test_f_asymp_unvac-point", shinyvalidate::sv_gte(0))
    iv$add_rule("test_f_asymp_unvac-range-1", shinyvalidate::sv_gte(0))
    iv$add_rule("test_f_asymp_unvac-range-2", shinyvalidate::sv_gte(0))
    # Vaccinated test frequency - required
    iv$add_rule("test_f_asymp_vac-point", shinyvalidate::sv_required())
    iv$add_rule("test_f_asymp_vac-range-1", shinyvalidate::sv_required())
    iv$add_rule("test_f_asymp_vac-range-2", shinyvalidate::sv_required())
    # Vaccinated test frequency - non-negative
    iv$add_rule("test_f_asymp_vac-point", shinyvalidate::sv_gte(0))
    iv$add_rule("test_f_asymp_vac-range-1", shinyvalidate::sv_gte(0))
    iv$add_rule("test_f_asymp_vac-range-2", shinyvalidate::sv_gte(0))
    # Case rate - required
    iv$add_rule("inf_r_incid-point", shinyvalidate::sv_required())
    iv$add_rule("inf_r_incid-range-1", shinyvalidate::sv_required())
    iv$add_rule("inf_r_incid-range-2", shinyvalidate::sv_required())
    # Case rate - non-negative
    iv$add_rule("inf_r_incid-point", shinyvalidate::sv_gte(0))
    iv$add_rule("inf_r_incid-range-1", shinyvalidate::sv_gte(0))
    iv$add_rule("inf_r_incid-range-2", shinyvalidate::sv_gte(0))
    iv$enable()

    # Input panel show/hide and info
    org_panel_server()
    comm_panel_server()
    advanced_panel_server()
    vac_eff_panel_server()
    test_panel_server()
    symp_panel_server()

    # Choose range or point based on `x_var`

    # Organization
    n_org              <- reactive_range("n_org")
    test_f_asymp_unvac <- reactive_range("test_f_asymp_unvac")
    test_f_asymp_vac   <- reactive_range("test_f_asymp_vac")
    vac_p_org          <- reactive_range("vac_p_org")
    # Community
    inf_r_incid <- reactive_range("inf_r_incid")
    vac_p_comm  <- reactive_range("vac_p_comm")
    # Advanced - vaccine efficacy
    vac_eff <- reactive_range("vac_eff")
    # Advanced - testing
    detect_sens <- reactive_range("detect_sens")
    detect_spec <- reactive_range("detect_spec")
    test_p_symp <- reactive_range("test_p_symp")
    # Advanced - symptoms
    inf_t_symp       <- reactive_range("inf_t_symp")
    inf_t_presymp    <- reactive_range("inf_t_presymp")
    symp_p_inf_unvac <- reactive_range("symp_p_inf_unvac")
    symp_p_inf_vac   <- reactive_range("symp_p_inf_vac")
    symp_p_uninf     <- reactive_range("symp_p_uninf")


    # Organization size
    n <- reactiveValues(org = n_org)

    # Distribution arguments
    dist_args <- reactiveValues()
    # Vaccination
    dist_args$vac <- reactive(list(
      p_comm = vac_p_comm(),
      p_org  = vac_p_org(),
      eff    = vac_eff()
    ), label = "dist_args$vac()")
    # Infection
    dist_args$inf <- reactive(list(
      r_incid   = inf_r_incid(),
      t_symp    = inf_t_symp(),
      t_presymp = inf_t_presymp()
    ), label = "dist_args$inf()")
    # Symptoms
    dist_args$symp <- reactive(list(
      p_inf_vac   = symp_p_inf_vac(),
      p_inf_unvac = symp_p_inf_unvac(),
      p_uninf     = symp_p_uninf()
    ), label = "dist_args$symp()")
    # Testing
    dist_args$test <- reactive(list(
      p_symp        = test_p_symp(),
      f_asymp_vac   = test_f_asymp_vac(),
      f_asymp_unvac = test_f_asymp_unvac()
    ), label = "dist_args$test()")
    # Detection
    dist_args$detect <- reactive(list(
      sens = detect_sens(),
      spec = detect_spec()
    ), label = "dist_args$detect()")

    # Plotting variables
    vars <- reactiveValues(
      x = reactive(input$x_var),
      y = reactive(input$y_var)
    )

    # Return named list of reactiveValues objects
    list(n = n, dist_args = dist_args, vars = vars)
  })
}


# Helpers (selectors) ----------------------------------------------------------


#' Select an Input Variable
#'
#' Create a dropdown UI element for selecting an input variable to profile over
#'
#' @param id The HTML element id
#' @param label The UI element label
#' @param width the UI element width
#'
#' @return A UI element
select_input <- function(id, label = "Input (X-axis)", width = "21rem") {
  selectInput(
    id,
    label = label,
    width = width,
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


#' Select an Output Type for Plotting
#'
#' @param id An HTML id for the UI element
#'
#' @return A string
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


# Helpers (wrappers) -----------------------------------------------------------


#' Wrap `conditonalNumericRangeInput()` for Ease of Use
#'
#' Wraps the `conditonalNumericRangeInput()` defined elsewhere in covidtest for
#' brevity.
#'
#' @inheritParam conditonalNumericRangeInput
#' @param br Add a line break below the input element
#'
#' @inherit return conditonalNumericRangeInput
num_input2 <- function(
  id,
  label,
  value,
  value2,
  min = 0,
  max = NA,
  step = 1,
  trigger = "x_var",
  br = TRUE
) {
  id_quo <- rlang::enquo(id)
  ns <- if (rlang::quo_is_call(id_quo)) rlang::call_fn(id_quo) else NS(NULL)
  tagList(
    conditionalNumericRangeInput(
      id = id,
      label = label,
      value = value,
      value2 = value2,
      min = min,
      max = max,
      step = step,
      trigger = trigger,
      ns = ns
    ),
    if (br) tags$br()
  )
}


#' Wrap `conditionalSliderRangeInput()` for Ease of Use
#'
#' Wraps the `conditonalSliderRangeInput()` defined elsewhere in covidtest for
#' brevity.
#'
#' @inheritParam conditonalSliderRangeInput
#' @param br Add a line break below the input element
#'
#' @inherit return conditonalSliderRangeInput
slider_pct2 <- function(
  id,
  label,
  value = 50,
  value2 = c(0, 100),
  min = 0,
  max = 100,
  step = 1,
  trigger = "x_var",
  prefix = NULL,
  br = TRUE
) {
  id_quo <- rlang::enquo(id)
  ns <- if (rlang::quo_is_call(id_quo)) rlang::call_fn(id_quo) else NS(NULL)
  tagList(
    conditionalSliderRangeInput(
      id = id,
      label = label,
      value = value,
      value2 = value2,
      trigger = trigger,
      ns = ns,
      min = min,
      max = max,
      step = step,
      prefix = prefix,
      suffix = "%"
    ),
    if (br) tags$br()
  )
}


#' Wrap `reactivePointRange()` for Consistent Nomenclature in Main App
#'
#' @inheritParams reactivePointRange
#'
#' @inherit return reactivePointRange
reactive_range <- function(id) {
  reactivePointRange(id)
}
