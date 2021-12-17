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
    toggle_panel(ns("org_link"),
                 init_visible = TRUE,
                 label = "Organization",
                 icon = icon("user-shield"),
                 tag_fn = h5,
      # People in organization
      num_input(ns("n_org"), "Organization Size (People)", 1e3),
      # Unvaccinated testing frequency
      num_input(ns("test_f_asymp_unvac"), "Unvaccinated Testing (Days)", 7),
      # Vaccinated testing frequency
      num_input(ns("test_f_asymp_vac"), "Vaccinated Testing (Days)", 0),
      # % Vaccinated in organization
      slider_pct(ns("vac_p_org"), "% Vaccinated (in Organization)"),
      tags$br()
    ),
    # Community inputs (local context)
    toggle_panel(ns("comm_link"),
                 init_visible = TRUE,
                 label = "Community",
                 icon = icon("users"),
                 tag_fn = h5,
      # Case rate per 100k per day in community
      num_input(ns("inf_r_incid"), "Case Rate (per 100k per Day)", 250),
      # % Vaccinated in community
      slider_pct(ns("vac_p_comm"), "% Vaccinated (in Community)"),
      tags$br()
    ),
    # Advanced inputs (illness, tests, and vaccinations)
    toggle_panel(ns("advanced_link"),
                 init_visible = TRUE,
                 label = "Advanced",
                 icon = icon("cog"),
                 tag_fn = h5,
      # Vaccine efficacy
      toggle_panel(ns("vac_eff_link"),
                   init_visible = FALSE,
                   label = "Vaccine Efficacy",
                   icon = icon("syringe"),
                   tag_fn = h6,
        slider_pct(ns("vac_eff"), label = NULL, value = 70),
        tags$br()
      ),
      # Test-related inputs
      toggle_panel(ns("test_link"),
                   init_visible = FALSE,
                   label = "Testing",
                   icon = icon("vial"),
                   tag_fn = h6,
        tags$br(),
        # Test sensitivity
        slider_pct(ns("detect_sens"),
                   label = "Test Sensitivity (%)",
                   value = 85
        ),
        # Test specificity
        slider_pct(ns("detect_spec"),
                   label = "Test Specificity (%)",
                   value = 99.5,
                   min   = 90,
                   step  = 0.1
        ),
        # % of symptomatic people that are tested
        slider_pct(ns("test_p_symp"),
                   label = "% Symptomatics Tested",
                   value = 95
        )
      ),
      # Symptom-related inputs
      toggle_panel(ns("symp_link"),
                   init_visible = FALSE,
                   label  = "Symptoms",
                   icon   = icon("head-side-cough"),
                   tag_fn = h6,
        tags$br(),
        # Symptomatic period
        num_input(ns("inf_t_symp"),
                  label = "Symptomatic Period (Days)",
                  value = 10
        ),
        # Pre-symptomatic period
        num_input(ns("inf_t_presymp"),
                  label = "Pre-Symptomatic Period (Days)",
                  value = 3
        ),
        # % of unvaccinated cases that are symptomatic
        slider_pct(ns("symp_p_inf_unvac"),
                   label = "% Symptomatic: Unvaccinated Cases"
        ),
        # % of vaccinated cases that are symptomatic
        slider_pct(ns("symp_p_inf_vac"),
                   label = "% Symptomatic: Vaccinated Cases",
                   value = 30
        ),
        # % of non-cases that are symptomatic
        slider_pct(ns("symp_p_uninf"),
                   label = "% Symptomatic: Non-Cases",
                   value = 2,
                   max   = 10,
                   step  = 0.1
        ),
        tags$br()
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

    # Show/hide input panels
    toggle_panel_server("org_link", input, time = 0.5)
    toggle_panel_server("comm_link", input, time = 0.3)
    toggle_panel_server("advanced_link", input, time = 0.4)
    toggle_panel_server("vac_eff_link", input, time = 0.2)
    toggle_panel_server("test_link", input, time = 0.4)
    toggle_panel_server("symp_link", input, time = 0.6)


    # Organizational inputs info
    observeEvent(input$org_link_info, ct_info_server(p(HTML(
      "Organizational inputs directly involve the organization. They are",
      " <b>interventions</b> that can be used to keep COVID-19 under control",
      " in the organization."
    ))))

    # Community inputs info
    observeEvent(input$comm_link_info, ct_info_server(p(
      "Community inputs define the context in which the organization resides.",
      " They help determine the baseline infection risk of an organization."
    )))

    # Advanced inputs info
    observeEvent(input$advanced_link_info, ct_info_server(p(HTML(
      "Advanced inputs are related to properties of COVID-19, its tests, and",
      " it vaccinations. They are 'advanced' in the sense that the average",
      " user is likely not an expert on COVID-19, and may not know reasonable",
      " values for these inputs. The defaults should suffice for many",
      " settings, but users are encouraged to change the defaults if they do",
      " not match your setting. The <b>Inputs</b> tab allows users to explore",
      " the effects of these parameters (and others) in detail."
    ))))

    # Input validation
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("n_org", shinyvalidate::sv_required())
    iv$add_rule("n_org", shinyvalidate::sv_gte(0))
    iv$add_rule("test_f_asymp_vac", shinyvalidate::sv_required())
    iv$add_rule("test_f_asymp_vac", shinyvalidate::sv_gte(0))
    iv$add_rule("test_f_asymp_vac", sv_not_lt_1())
    iv$add_rule("test_f_asymp_unvac", shinyvalidate::sv_required())
    iv$add_rule("test_f_asymp_unvac", shinyvalidate::sv_gte(0))
    iv$add_rule("test_f_asymp_unvac", sv_not_lt_1())
    iv$add_rule("inf_r_incid", shinyvalidate::sv_required())
    iv$add_rule("inf_r_incid", shinyvalidate::sv_between(0, 1e5,
      message_fmt = "Must be between 0 and 100,000"))
    iv$add_rule("inf_t_symp", shinyvalidate::sv_required())
    iv$add_rule("inf_t_symp", shinyvalidate::sv_gte(0))
    iv$add_rule("inf_t_presymp", shinyvalidate::sv_required())
    iv$add_rule("inf_t_presymp", shinyvalidate::sv_gte(0))
    iv$enable()

    n_org <- reactive(req_gte(input$n_org, 0))
    test_f_asymp_vac <- reactive(req_gte(input$test_f_asymp_vac, 0))
    test_f_asymp_unvac <- reactive(req_gte(input$test_f_asymp_unvac, 0))
    inf_r_incid <- reactive(req_between(input$inf_r_incid, 0, 1e5))
    inf_t_symp <- reactive(req_gte(input$inf_t_symp, 0))
    inf_t_presymp <- reactive(req_gte(input$inf_t_presymp, 0))

    dist_args <- reactiveValues()

    dist_args$n <- reactive(n_org())

    dist_args$vac <- reactive(list(
      p_comm = input$vac_p_comm * 1e-2,
      p_org  = input$vac_p_org  * 1e-2,
      eff    = input$vac_eff    * 1e-2
    ))

    dist_args$inf <- reactive(list(
      p_incid   = inf_r_incid() / 1e5,
      t_symp    = inf_t_symp(),
      t_presymp = inf_t_presymp()
    ))

    dist_args$symp <- reactive(list(
      p_inf_vac   = input$symp_p_inf_vac   * 1e-2,
      p_inf_unvac = input$symp_p_inf_unvac * 1e-2,
      p_uninf     = input$symp_p_uninf     * 1e-2
    ))


    dist_args$test <- reactive(list(
      p_symp        = input$test_p_symp * 1e-2,
      p_asymp_vac   = correct_freq(1 / test_f_asymp_vac()),
      p_asymp_unvac = correct_freq(1 / test_f_asymp_unvac())
    ))

    dist_args$detect <- reactive(list(
      sens = input$detect_sens * 1e-2,
      spec = input$detect_spec * 1e-2
    ))

    dist_args
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

  if (NROW(label) > 0 && nchar(label) > 0) {
    label <- tags$span(label, ct_info_ui(paste0(id, "_info")))
  }

  input <- numericInput(id,
    label = NULL, width = width,
    value = value, min = min, max = max, step = step
  )

  tagList(tags$label(label), input)
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

  if (NROW(label) > 0 && nchar(label) > 0) {
    label <- tags$span(label, ct_info_ui(paste0(id, "_info")))
  }

  sliderInput(id,
    label = label,
    value = value, step = step, width = width, pre = pre,
    min = min, max = max, post = "%", ticks = FALSE
  )
}

action_link <- function(id, label, icon = NULL, tag_fn = NULL) {

  icon_label_info <- tags$span(icon, label, ct_info_ui(paste0(id, "_info")))

  a_link <- actionLink(id, label = icon_label_info)

  if (is.null(tag_fn)) a_link else tag_fn(a_link)
}

correct_freq <- function(x) {
  fcase(
    is.infinite(x),     0,
    x > 1,              1,
    rep(TRUE, NROW(x)), x
  )
}

sv_not_lt_1 <- function(
  left = 0,
  right = 1,
  message_fmt = "Testing more than once a day does not change results."
) {
  force(left)
  force(right)
  force(message_fmt)
  function(x) {
    if (left < x & x < right) {
      glue::glue_data_safe(
        list(left = left, right = right),
        message_fmt
      )
    }
  }
}

req_gte <- function(x, rhs) {
  req(x, x >= rhs, cancelOutput = TRUE)
}

req_between <- function(x, left, right) {
  req(x, left <= x, x <= right, cancelOutput = TRUE)
}
