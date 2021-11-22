scenarios_input <- function(id) {
  ns <- NS(id)

  tagList(
    shinyWidgets::chooseSliderSkin("Flat", color = "#29434e"),
    actionLink(ns("input_org_link"), label = h5(icon("user-shield"), "Organization")),
    conditionalPanel("input.input_org_link % 2 == 0", ns = ns,
      num_input(ns("input_freq_vac"),   "Vaccinated Testing (Days)"),
      num_input(ns("input_freq_unvac"), "Unvaccinated Testing (Days)"),
      slider_pct(ns("input_org_vac"), "% Vaccinated (in Organization)"),
      tags$br()),
    actionLink(ns("input_comm_link"), label = h5(icon("users"), "Community")),
    conditionalPanel("input.input_comm_link % 2 == 0", ns = ns,
      num_input(ns("input_case_rate"), "Case Rate (per 100k per Day)"),
      slider_pct(ns("input_comm_vac"), "% Vaccinated (in Community)"),
      tags$br()),
    actionLink(ns("input_advanced_link"), label = h5(icon("cog"), "Advanced")),
    conditionalPanel("input.input_advanced_link % 2 == 0", ns = ns,
      tags$br(),
      tags$div(
        actionLink(ns("input_vac_eff_link"), label = "Vaccine Efficacy", icon = icon("syringe")),
        ct_info_ui(ns("input_vac_eff_info"))),
      conditionalPanel("input.input_vac_eff_link % 2 == 1", ns = ns,
        slider_pct(ns("input_vac_eff"), label = NULL)),
      tags$br(),
      tags$div(
        actionLink(ns("input_symp_link"), label = "Symptoms", icon = icon("head-side-cough")),
        ct_info_ui(ns("input_symp_info"))),
      conditionalPanel("input.input_symp_link % 2 == 1", ns = ns,
        tags$br(),
        num_input(ns("input_t_symp"),
          label = tags$span("Symptomatic Period (Days)", ct_info_ui(ns("input_t_symp_info"))),
          value = 10),
        num_input(ns("input_t_presymp"),
          label = tags$span("Pre-Symptomatic Period (Days)", ct_info_ui(ns("input_t_presymp_info"))),
          value = 3),
        slider_pct(ns("input_p_symp_unvac"),
          label = tags$span("% Symptomatic: Unvaccinated Cases", ct_info_ui(ns("input_p_symp_unvac_info")))),
        slider_pct(ns("input_p_symp_vac"),
          label = tags$span("% Symptomatic: Vaccinated Cases", ct_info_ui(ns("input_p_symp_vac_info"))),
          value = 30),
        slider_pct(ns("input_p_symp_uninf"),
          label = tags$span("% Symptomatic: Non-Cases", ct_info_ui(ns("input_p_symp_uninf_info"))),
          value = 2, max = 10, step = 0.1)
      ),
      tags$br(),
      tags$div(
        actionLink(ns("input_test_link"), label = "Testing", icon = icon("vial")),
        ct_info_ui(ns("input_test_info"))),
      conditionalPanel("input.input_test_link % 2 == 1", ns = ns,
        tags$br(),
        slider_pct(ns("input_test_sens"),
          label = tags$span("Test Sensitivity (%)", ct_info_ui(ns("input_test_sens_info"))),
          value = 85, min = 50),
        slider_pct(ns("input_test_spec"),
          label = tags$span("Test Specificity (%)", ct_info_ui(ns("input_test_spec_info"))),
          value = 99.5, min = 90, step = 0.1),
        slider_pct(ns("input_test_symp"),
          label = tags$span("% Symptomatics Tested", ct_info_ui(ns("input_test_symp_info"))),
          value = 95)
        )
    )
  )
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
  sliderInput(id,
    label = label, value = value, step = step, width = width, pre = pre,
    min = min, max = max, post = "%", ticks = FALSE)
}
