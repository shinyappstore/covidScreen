#' scenarios_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_scenarios_input_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Make sliders nicer-looking
    shinyWidgets::chooseSliderSkin("Flat", color = "#29434e"),
    # Organizational inputs (interventions)
    conditional_panel(ns("input_org_link"), init_visible = TRUE,
      label = "Organization", icon = icon("user-shield"), tag_fn = h5,
      # Vaccinated testing frequency
      num_input(ns("input_test_f_asymp_vac"), "Vaccinated Testing (Days)", 0),
      # Unvaccinated testing frequency
      num_input(ns("input_test_f_asymp_unvac"), "Unvaccinated Testing (Days)"),
      # % Vaccinated in organization
      slider_pct(ns("input_vac_p_org"), "% Vaccinated (in Organization)"),
      tags$br()),
    # Community inputs (local context)
    conditional_panel(ns("input_comm_link"), init_visible = TRUE,
      label = "Community", icon = icon("users"), tag_fn = h5,
      # Case rate per 100k per day in community
      num_input(ns("input_inf_r_incid"), "Case Rate (per 100k per Day)"),
      # % Vaccinated in community
      slider_pct(ns("input_vac_p_comm"), "% Vaccinated (in Community)"),
      tags$br()),
    # Advanced inputs (illness, tests, and vaccinations)
    conditional_panel(ns("input_advanced_link"), init_visible = TRUE,
      label = "Advanced", icon = icon("cog"), tag_fn = h5,
      # Vaccine efficacy
      conditional_panel(ns("input_vac_eff_link"), init_visible = FALSE,
        label = "Vaccine Efficacy", icon = icon("syringe"), tag_fn = h6,
        slider_pct(ns("input_vac_eff"), label = NULL),
        tags$br()),
      # Symptom-related inputs
      conditional_panel(ns("input_symp_link"), init_visible = FALSE,
        label = "Symptoms", icon = icon("head-side-cough"), tag_fn = h6,
        tags$br(),
        # Symptomatic period
        num_input(ns("input_inf_t_symp"),
          label = "Symptomatic Period (Days)", value = 10),
        # Pre-symptomatic period
        num_input(ns("input_inf_t_presymp"),
          label = "Pre-Symptomatic Period (Days)", value = 3),
        # % of unvaccinated cases that are symptomatic
        slider_pct(ns("input_symp_p_inf_unvac"),
          label = "% Symptomatic: Unvaccinated Cases"),
        # % of vaccinated cases that are symptomatic
        slider_pct(ns("input_symp_p_inf_vac"),
          label = "% Symptomatic: Vaccinated Cases", value = 30),
        # % of non-cases that are symptomatic
        slider_pct(ns("input_symp_p_uninf"),
          label = "% Symptomatic: Non-Cases", value = 2, max = 10, step = 0.1),
        tags$br()),
      # Test-related inputs
      conditional_panel(ns("input_test_link"), init_visible = FALSE,
        label = "Testing", icon = icon("vial"), tag_fn = h6,
        tags$br(),
        # Test sensitivity
        slider_pct(ns("input_detect_sens"),
          label = "Test Sensitivity (%)", value = 85, min = 50),
        # Test specificity
        slider_pct(ns("input_detect_spec"),
          label = "Test Specificity (%)", value = 99.5, min = 90, step = 0.1),
        # % of symptomatic people that are tested
        slider_pct(ns("input_test_p_symp"),
          "% Symptomatics Tested", value = 95)
        )
    )
  )
}

#' scenarios_input Server Functions
#'
#' @noRd
mod_scenarios_input_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Organizational inputs info - NOT WORKING
    observeEvent(input$input_org_link_info, ct_info_server(p(HTML(
      "These are inputs that directly involve the organization. They are",
      " <b>interventions</b> that can be used to keep COVID-19 under control",
      " in the organization."
    ))))

    vac <- reactive(list(
      p_comm = input$input_vac_p_comm * 1e-2,
      p_org  = input$input_vac_p_org  * 1e-2,
      eff    = input$input_vac_eff    * 1e-2
    ))

    inf <- reactive(list(
      p_incid   = input$input_inf_r_incid / 1e5,
      t_symp    = input$input_inf_t_symp,
      t_presymp = input$input_inf_t_presymp
    ))

    symp <- reactive(list(
      p_inf_vac   = input$input_symp_p_inf_vac   * 1e-2,
      p_inf_unvac = input$input_symp_p_inf_unvac * 1e-2,
      p_uninf     = input$input_symp_p_uninf     * 1e-2
    ))

    test <- reactive(list(
      p_symp        = input$input_test_p_symp * 1e-2,
      p_asymp_vac   = 1 / input$input_test_f_asymp_vac,
      p_asymp_unvac = 1 / input$input_test_f_asymp_unvac
    ))

    detect <- reactive(list(
      sens = input$input_detect_sens * 1e-2,
      spec = input$input_detect_spec * 1e-2
    ))

    reactive(list(
      vac = vac(), inf = inf(), symp = symp(), test = test(), detect = detect()
      ))
  })
}

num_input <- function(
  id,
  label,
  value = 0,
  min = 0,
  max = NA,
  step = 1,
  width = "120px"
) {

  if (length(label) > 0) {
    label <- tags$span(label, ct_info_ui(paste0(id, "_info")))
  }

  tagList(
    tags$label(label),
    numericInput(id,
      label = NULL, width = width,
      value = value, min = min, max = max, step = step)
  )
}

slider_pct <- function(
  id,
  label,
  value = 50,
  step = 1,
  min = 0,
  max = 100,
  width = NULL,
  pre = NULL
) {

  if (length(label) > 0) {
    label <- tags$span(label, ct_info_ui(paste0(id, "_info")))
  }

  sliderInput(id,
    label = label,
    value = value, step = step, width = width, pre = pre,
    min = min, max = max, post = "%", ticks = FALSE)
}

conditional_panel <- function(
  id,
  ...,
  label = NULL,
  icon = NULL,
  tag_fn = NULL,
  init_visible = FALSE
) {

  id_quo <- rlang::enquo(id)

  if (rlang::quo_is_call(id_quo)) {
    ns <- rlang::call_fn(id_quo)
    id_chr <- eval(rlang::call_args(id_quo)[[1]])
  } else {
    ns <- NS(NULL)
    id_chr <- id
  }

  condition <- paste0(
    "input.", id_chr, " % 2 == ", if (init_visible) "0" else "1"
  )

  tags$div(
    action_link(id, label = label, icon = icon, tag_fn = tag_fn),
    conditionalPanel(condition = condition, ns = ns, ...)
  )
}

action_link <- function(id, label, icon = NULL, tag_fn = NULL) {

  icon_label_info  <- tags$span(icon, label, ct_info_ui(paste0(id, "_info")))

  a_link <- actionLink(id, label = icon_label_info)

  if (is.null(tag_fn)) a_link else tag_fn(a_link)
}
