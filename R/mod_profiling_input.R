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
        num_input2(ns("n_org"),
                   label = "Organization Size (People)",
                   value = 1e3,
                   value2 = c(1e2, 1e3)
        )
        # # Unvaccinated testing frequency
        # num_input2(ns("test_f_asymp_unvac"),
        #            label = "Unvaccinated Testing (Days)",
        #            value = 7,
        #            value2 = c(1, 7)
        # ),
        # # Vaccinated testing frequency
        # num_input2(ns("test_f_asymp_vac"),
        #            label = "Vaccinated Testing (Days)",
        #            value = 0,
        #            value2 = c(1, 7)
        # ),
        # # % Vaccinated in organization
        # slider_pct2(ns("vac_p_org"),
        #            label = "% Vaccinated (in Organization)"
        # )
      ))
      # # Community
      # column(width = 4, toggle_panel(ns("comm_link"),
      #                                init_visible = TRUE,
      #                                label = "Community",
      #                                icon = icon("users"),
      #                                tag_fn = h5,
      #   # Case rate per 100k per day in community
      #   num_input2(ns("inf_r_incid"),
      #              label = "Case Rate (per 100k per Day)",
      #              value = 250,
      #              value2 = c(0, 1e3)
      #   ),
      #   # % Vaccinated in community
      #   slider_pct2(ns("vac_p_comm"),
      #              label = "% Vaccinated (in Community)"
      #   )
      # )),
      # # Advanced
      # column(width = 4, toggle_panel(ns("advanced_link"),
      #                                init_visible = TRUE,
      #                                label = "Advanced",
      #                                icon = icon("cog"),
      #                                tag_fn = h5,
      #   # Vaccine efficacy
      #   toggle_panel(ns("vac_eff_link"),
      #               init_visible = FALSE,
      #               label = "Vaccine Efficacy",
      #               icon = icon("syringe"),
      #               tag_fn = h6,
      #     slider_pct2(ns("vac_eff"),
      #                label = NULL,
      #                value = 70
      #     ),
      #     tags$br()
      #   ),
      #   # Test-related inputs
      #   toggle_panel(ns("test_link"),
      #               init_visible = FALSE,
      #               label = "Testing",
      #               icon = icon("vial"),
      #               tag_fn = h6,
      #               tags$br(),
      #     # Test sensitivity
      #     slider_pct2(ns("detect_sens"),
      #                label = "Test Sensitivity (%)",
      #                value = 85,
      #                value2 = c(50, 100)
      #     ),
      #     # Test specificity
      #     slider_pct2(ns("detect_spec"),
      #                label = "Test Specificity (%)",
      #                value = 99.5,
      #                value2 = c(90, 100),
      #                min   = 90,
      #                step  = 0.1
      #     ),
      #     # % of symptomatic people that are tested
      #     slider_pct2(ns("test_p_symp"),
      #                label = "% Symptomatics Tested",
      #                value = 95,
      #                value2 = c(50, 100)
      #     )
      #   ),
      #   # Symptom-related inputs
      #   toggle_panel(ns("symp_link"),
      #               init_visible = FALSE,
      #               label  = "Symptoms",
      #               icon   = icon("head-side-cough"),
      #               tag_fn = h6,
      #               tags$br(),
      #     # Symptomatic period
      #     num_input2(ns("inf_t_symp"),
      #               label = "Symptomatic Period (Days)",
      #               value = 10,
      #               value2 = c(7, 14)
      #     ),
      #     # Pre-symptomatic period
      #     num_input2(ns("inf_t_presymp"),
      #               label = "Pre-Symptomatic Period (Days)",
      #               value = 3,
      #               value2 = c(1, 5)
      #     ),
      #     # % of unvaccinated cases that are symptomatic
      #     slider_pct2(ns("symp_p_inf_unvac"),
      #                label = "% Symptomatic: Unvaccinated Cases",
      #                value2 = c(25, 75)
      #     ),
      #     # % of vaccinated cases that are symptomatic
      #     slider_pct2(ns("symp_p_inf_vac"),
      #                label = "% Symptomatic: Vaccinated Cases",
      #                value = 30,
      #                value2 = c(10, 50)
      #     ),
      #     # % of non-cases that are symptomatic
      #     slider_pct2(ns("symp_p_uninf"),
      #                label = "% Symptomatic: Non-Cases",
      #                value = 2,
      #                value2 = c(0, 10),
      #                max   = 10,
      #                step  = 0.1
      #     )
      #   )
      # ))
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
    # toggle_panel_server("comm_link", input, time = 0.3)
    # toggle_panel_server("advanced_link", input, time = 0.4)
    # toggle_panel_server("vac_eff_link", input, time = 0.2)
    # toggle_panel_server("test_link", input, time = 0.4)
    # toggle_panel_server("symp_link", input, time = 0.6)

    # Organizational inputs info
    observeEvent(input$org_link_info, ct_info_server(p(HTML(
      "Organizational inputs directly involve the organization. They are",
      " <b>interventions</b> that can be used to keep COVID-19 under control",
      " in the organization."
    ))), label = "org_link_info_server()")

    # # Community inputs info
    # observeEvent(input$comm_link_info, ct_info_server(p(
    #   "Community inputs define the context in which the organization resides.",
    #   " They help determine the baseline infection risk of an organization."
    # )), label = "comm_link_info_server()")
    #
    # # Advanced inputs info
    # observeEvent(input$advanced_link_info, ct_info_server(p(HTML(
    #   "Advanced inputs are related to properties of COVID-19, its tests, and",
    #   " it vaccinations. They are 'advanced' in the sense that the average",
    #   " user is likely not an expert on COVID-19, and may not know reasonable",
    #   " values for these inputs. The defaults should suffice for many",
    #   " settings, but users are encouraged to change the defaults if they do",
    #   " not match your setting. The <b>Inputs</b> tab allows users to explore",
    #   " the effects of these parameters (and others) in detail."
    # ))), label = "advanced_link_info_server()")

    # Input validation
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("n_org", shinyvalidate::sv_required())
    iv$add_rule("n_org", sv_gte_multi(0))
    # iv$add_rule("test_f_asymp_vac", shinyvalidate::sv_required())
    # iv$add_rule("test_f_asymp_vac", sv_gte_multi(0))
    # iv$add_rule("test_f_asymp_vac", sv_not_lt_1())
    # iv$add_rule("test_f_asymp_unvac", shinyvalidate::sv_required())
    # iv$add_rule("test_f_asymp_unvac", sv_gte_multi(0))
    # iv$add_rule("test_f_asymp_unvac", sv_not_lt_1())
    # iv$add_rule("inf_r_incid", shinyvalidate::sv_required())
    # iv$add_rule("inf_r_incid", shinyvalidate::sv_between(0, 1e5,
    #                                                      message_fmt = "Must be between 0 and 100,000"))
    # iv$add_rule("inf_t_symp", shinyvalidate::sv_required())
    # iv$add_rule("inf_t_symp", sv_gte_multi(0))
    # iv$add_rule("inf_t_presymp", shinyvalidate::sv_required())
    # iv$add_rule("inf_t_presymp", sv_gte_multi(0))
    # iv$enable()

    n_org <- reactive(
      req_gte(get_input(input, "n_org")(), 0),
      label = "n_org()"
    )
    # test_f_asymp_vac <- reactive(
    #   req_gte(get_input(input, "test_f_asymp_vac")(), 0),
    #   label = "test_f_asymp_vac()"
    # )
    # test_f_asymp_unvac <- reactive(
    #   req_gte(get_input(input, "test_f_asymp_unvac")(), 0),
    #   label = "test_f_asymp_unvac()"
    # )
    # inf_r_incid <- reactive(
    #   req_between(get_input(input, "inf_r_incid")(), 0, 1e5),
    #   label = "inf_r_incid()"
    # )
    # inf_t_symp <- reactive(
    #   req_gte(get_input(input, "inf_t_symp")(), 0),
    #   label = "inf_t_symp()"
    # )
    # inf_t_presymp <- reactive(
    #   req_gte(get_input(input, "inf_t_presymp")(), 0),
    #   label = "inf_t_presymp()"
    # )

    # vac_p_org  <- get_input(input, "vac_p_org")
    # vac_p_comm <- get_input(input, "vac_p_comm")
    # vac_eff    <- get_input(input, "vac_eff")
    # detect_sens <- get_input(input, "detect_sens")
    # detect_spec <- get_input(input, "detect_spec")
    # test_p_symp <- get_input(input, "test_p_symp")
    # symp_p_inf_unvac <- get_input(input, "symp_p_inf_unvac")
    # symp_p_inf_vac   <- get_input(input, "symp_p_inf_vac")
    # symp_p_uninf     <- get_input(input, "symp_p_uninf")

    dist_args <- reactiveValues()

    dist_args$vac <- reactive(list(
      p_comm = vac_p_comm() * 1e-2,
      p_org  = vac_p_org()  * 1e-2,
      eff    = vac_eff()    * 1e-2
    ), label = "dist_args$vac()")

    dist_args$inf <- reactive(list(
      p_incid   = inf_r_incid() / 1e5,
      t_symp    = inf_t_symp(),
      t_presymp = inf_t_presymp()
    ), label = "dist_args$inf()")

    dist_args$symp <- reactive(list(
      p_inf_vac   = symp_p_inf_vac()   * 1e-2,
      p_inf_unvac = symp_p_inf_unvac() * 1e-2,
      p_uninf     = symp_p_uninf()     * 1e-2
    ), label = "dist_args$symp()")


    dist_args$test <- reactive(list(
      p_symp        = test_p_symp() * 1e-2,
      f_asymp_vac   = test_f_asymp_vac(),
      f_asymp_unvac = test_f_asymp_unvac()
    ), label = "dist_args$test()")

    dist_args$detect <- reactive(list(
      sens = detect_sens() * 1e-2,
      spec = detect_spec() * 1e-2
    ), label = "dist_args$detect()")

    vars <- reactiveValues(
      x = reactive(input$x_var, label = "vars$x()"),
      y = reactive(input$y_var, label = "vars$y()")
    )

    n <- reactiveValues(org = reactive(n_org(), label = "n$org()"))

    list(n = n, dist_args = dist_args, vars = vars)
  })
}

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


num_input2 <- function(
  id,
  label,
  value = 0,
  value2 = value,
  min = 0,
  max = NA,
  step = 1,
  separator = span(style = "margin-left: 0.5rem; margin-right: 0.5rem", "to"),
  trigger = "x_var"
) {

  id_quo <- rlang::enquo(id)
  if (rlang::quo_is_call(id_quo)) {
    id_chr <- rlang::call_args(id_quo)[[1]]
    ns <- rlang::call_fn(id_quo)
  } else {
    id_chr <- id
    ns <- NS(NULL)
  }

  if (NROW(label) > 0 && nchar(label) > 0) {
    label <- tags$span(label, ct_info_ui(paste0(id, "_info")))
  }

  span(
    conditionalPanel(
      tags$label(label),
      condition = paste0("input.", trigger, " != '", id_chr, "'"),
      ns = ns,
      numericInput(id,
        label = NULL, width = "120px",
        value = value, min = min, max = max, step = step
      )
    ),
    conditionalPanel(
      tags$label(tags$b(label)),
      condition = paste0("input.", trigger, " == '", id_chr, "'"),
      ns = ns,
      shinyWidgets::numericRangeInput(paste0(id, "2"),
        label = NULL, width = "240px", separator = separator,
        value = value2, min = min, max = max, step = step
      )
    )
  )
}


slider_pct2 <- function(
  id,
  label,
  value = 50,
  value2 = c(0, 100),
  step = 1,
  min = 0,
  max = 100,
  width = NULL,
  pre = NULL,
  trigger = "x_var"
) {

  id_quo <- rlang::enquo(id)
  if (rlang::quo_is_call(id_quo)) {
    id_chr <- rlang::call_args(id_quo)[[1]]
    ns <- rlang::call_fn(id_quo)
  } else {
    id_chr <- id
    ns <- NS(NULL)
  }

  if (NROW(label) > 0 && nchar(label) > 0) {
    label <- tags$span(label, ct_info_ui(paste0(id, "_info")))
  }

  slider_fn <- purrr::partial(sliderInput,
    step = step, width = width, pre = pre,
    min = min, max = max, post = "%", ticks = FALSE
  )

  tagList(
    conditionalPanel(
      condition = paste0("input.", trigger, " != '", id_chr, "'"),
      ns = ns,
      slider_fn(inputId = id, value = value, label = label)
    ),
    conditionalPanel(
      condition = paste0("input.", trigger, " == '", id_chr, "'"),
      ns = ns,
      slider_fn(inputId = paste0(id, "2"), value = value2, label = tags$b(label))
    )
  )
}


get_input <- function(input, x, i_nm = "x_var") {
  reactive(
    if (input[[i_nm]] == x) input[[paste0(x, "2")]] else input[[x]],
    label = paste0(x, "()")
  )
}
