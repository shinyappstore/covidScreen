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
    # Make sliders nicer-looking
    shinyWidgets::chooseSliderSkin("Flat", color = "#29434e"),
    fluidRow(span(style = "margin-left: auto", select_input(ns("x_var"))),
             span(style = "width: 3rem"),
             span(style = "margin-right: auto", select_output(ns("y_var")))
    ),
    fluidRow(
      # Organization
      column(width = 4, toggle_panel(ns("org_link"),
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
        slider_pct(ns("vac_p_org"), "% Vaccinated (in Organization)")
      )),
      # Community
      column(width = 4, toggle_panel(ns("comm_link"),
                                     init_visible = TRUE,
                                     label = "Community",
                                     icon = icon("users"),
                                     tag_fn = h5,
        # Case rate per 100k per day in community
        num_input(ns("inf_r_incid"), "Case Rate (per 100k per Day)", 250),
        # % Vaccinated in community
        slider_pct(ns("vac_p_comm"), "% Vaccinated (in Community)")
      )),
      # Advanced
      column(width = 4, toggle_panel(ns("advanced_link"),
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
          )
        )
      ))
    )
  )
}

#' profiling_input Server Functions
#'
#' @noRd
mod_profiling_input_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Show/hide input panels
    toggle_panel_server("org_link", input, time = 0.5)
    toggle_panel_server("comm_link", input, time = 0.3)
    toggle_panel_server("advanced_link", input, time = 0.4)
    toggle_panel_server("vac_eff_link", input, time = 0.2)
    toggle_panel_server("test_link", input, time = 0.4)
    toggle_panel_server("symp_link", input, time = 0.6)
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
