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
      # Organization inputs
      org_profiling_column(ns("org_link"), ns = ns)
    )
  )
}

#' profiling_input2 Server Functions
#'
#' @noRd
mod_profiling_input2_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Validate inputs
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("n_org-point", shinyvalidate::sv_required())
    iv$add_rule("n_org-range-1", shinyvalidate::sv_required())
    iv$add_rule("n_org-range-2", shinyvalidate::sv_required())
    iv$enable()

    # Input panel show/hide and info
    org_panel_server()

    # Choose range or point based on `x_var`

    # Organization
    n_org              <- reactive_range("n_org")
    test_f_asymp_unvac <- reactive_range("test_f_asymp_unvac")
    test_f_asymp_vac   <- reactive_range("test_f_asymp_vac")
    vac_p_org          <- reactive_range("vac_p_org")

    # Organization size
    n <- reactiveValues(org = n_org)

    # Distribution arguments
    dist_args <- reactiveValues()
    dist_args$vac <- reactive(list(
      p_org = vac_p_org() * 1e-2
    ), label = "dist_args$vac()")
    # dist_args$inf <- reactive(list())
    # dist_args$symp <- reactive(list())
    dist_args$test <- reactive(list(
      f_asymp_unvac = test_f_asymp_unvac(),
      f_asymp_vac   = test_f_asymp_vac()
    ), label = "dist_args$test()")
    # dist_args$detect <- reactive(list())

    # Plotting variables
    plt_vars <- reactiveValues(
      x = reactive(input$x_var),
      y = reactive(input$y_var)
    )

    # Return named list of reactiveValues objects
    list(n = n, dist_args = dist_args, plt_vars = plt_vars)
  })
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

# Helpers (shinyvalidate) ------------------------------------------------------


sv_gte_multi <- function(
  rhs,
  message_fmt = "Must be greater than or equal to {rhs}.",
  allow_na = FALSE,
  allow_nan = FALSE,
  allow_inf = FALSE
) {
  shinyvalidate::sv_gte(
    rhs = rhs,
    message_fmt = message_fmt,
    allow_multiple = TRUE,
    allow_na = allow_na,
    allow_nan = allow_nan,
    allow_inf = allow_inf
  )
}
