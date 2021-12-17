#' scenarios_output UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_scenarios_output_ui <- function(id){
  ns <- NS(id)
  tagList(
    toggle_panel(ns("risk"),
                 init_visible = TRUE,
                 label = "Infection Risk (Undetected Cases)",
                 tag_fn = h3,
      fluidRow(
        column(width = 6, highchartOutput(ns("risk_plot"))),
        column(width = 6, uiOutput(ns("risk_expl")))
      )
    ),
    toggle_panel(ns("benefit"),
                 init_visible = TRUE,
                 label = "Testing Benefit (Undetected Case Reduction)",
                 tag_fn = h3,
      fluidRow(
        column(width = 6, highchartOutput(ns("benefit_plot"))),
        column(width = 6, uiOutput(ns("benefit_expl")))
      )
    ),
    toggle_panel(ns("vac"),
                 init_visible = TRUE,
                 label = "Testing Benefit (Vaccinated vs Unvaccinated)",
                 tag_fn = h3,
      fluidRow(
        column(width = 6, highchartOutput(ns("vac_plot"))),
        column(width = 6, uiOutput(ns("vac_expl")))
      )
    )
  )
}

#' scenarios_output Server Functions
#'
#' @noRd
mod_scenarios_output_server <- function(id, dist_args){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    data_test  <- reactive_dist(dist_args)
    data_test1 <- reactive_dist(const_testing(dist_args, 1, 1))
    data_test0 <- reactive_dist(const_testing(dist_args))


    # Infection risk (# of active cases)
    toggle_panel_server("risk", input)
    data_risk <- reactive(prep_risk(
      data_test = data_test(),
      data_no_test = data_test0(),
      n = dist_args$n()
    ), label = "prep_risk()")
    output$risk_plot <- renderHighchart(plot_risk(data_risk()))
    output$risk_expl <- renderUI(explain_risk(data_risk(), n = dist_args$n()))
    observeEvent(input$risk_info, risk_plot_info())

    # Testing benefit (reduction in active cases)
    toggle_panel_server("benefit", input)
    data_benefit <- reactive(prep_benefit(
      data_test = data_test(),
      data_no_test = data_test0(),
      n = dist_args$n()
    ))
    output$benefit_plot <- renderHighchart(plot_benefit(data_benefit()))
    output$benefit_expl <- renderUI(explain_benefit(data_benefit()))
    observeEvent(input$benefit_info, benefit_plot_info())

    # Vaccination comparison
    toggle_panel_server("vac", input)
    data_vac <- reactive(prep_vac(
      data_test0 = data_test0(),
      data_test1 = data_test1()
    ))
    output$vac_plot <- renderHighchart(plot_vac(data_vac()))
    output$vac_expl <- renderUI(explain_vac(data_vac()))
    observeEvent(input$vac_info, vac_plot_info())
  })
}

risk_plot_info <- function() {
  ct_info_server(
    tags$p(
      "Each dot represents a person in the organization.",
      tags$b(style = "color: #e57373", "Red"),
      "dots represent people who have COVID-19 and are not identified,",
      " i.e. infection risk.", tags$b(style = "color: #64b5f6", "Blue"),
      "dots represent people who have COVID-19 but have been identified by",
      " testing, i.e. no longer a risk.",
      tags$b(style = "color: #546e7a", "Blue-gray"), "and",
      tags$b(style = "color: #757575", "gray"), "dots represent people who",
      " would be detecting without testing or who do not have COVID-19."
    ),
    h6("Explanation"),
    tags$p(
      "Infection risk is controlled by the number of people in an organization",
      " who have COVID-19. We call these people active cases.",
      " For an active case to contribute to infection risk,",
      " they must be in physical proximity with others in the organization.",
      " This can be prevented by identifying and isolating cases quickly."
    )
  )
}

benefit_plot_info <- function() {
  ct_info_server(
    tags$p(
      "This bar represents all cases that would go undetected without regular",
      " testing. The ", tags$b(style = "color: #64b5f6", "blue"), "portion",
      " represents the cases detected by regular testing, i.e. the benefit.",
      " The", tags$b(style = "color: #e57373", "red"), "portion represents the",
      " cases that go undetected even with regular testing. The",
      tags$b(style = "color: #64b5f6", "blue"), "percentage is an estimate",
      " of the efficacy of a testing strategy."
    ),
    h6("Explanation"),
    tags$p(
      "The benefits of regular asymptomatic testing come through quickly",
      " identifying and isolating COVID-19 cases. The less time a case goes",
      " undetected, the less opportunity COVID-19 has to spread through them.",
      " Some cases will identify themselves when they become symptomatic, but",
      " many cases never develop symptoms. Even cases that develop symptoms",
      " are often most infectious just before showing symptoms."
    )
  )
}

vac_plot_info <- function() {
  ct_info_server(
    tags$p(
      "The bars represent the expected number of cases identified by testing",
      " 100 people with that vaccination status in the organization. The ",
      tags$b(style = "color: #64b5f6", "blue"), "bar represents detected cases",
      " in 100 vaccinated people; the ",
      tags$b(style = "color: #78909c", "blue-gray"), "bar represents detected",
      " cases in 100 unvaccinated people. The multiplier is the amount of",
      " additional benefit gained from testing the group with a higher",
      " detection percenage."
    ),
    h6("Explanation"),
    tags$p(
      "Both vaccinated and unvaccinated people contract COVID-19, but",
      " vaccinated people generally have a lower chance of doing so, and an",
      " even lower chance of showing symptoms. This means that vaccinated case",
      " rates tend be lower, but vaccinated cases are often harder to detect",
      " based on symptoms."
    )
  )
}

const_testing <- function(dist_args, p_vac = 0, p_unvac = 0) {
  if (is.reactivevalues(dist_args)) {
    c_test_args <- reactiveValues(
      n = dist_args$n,
      vac = dist_args$vac,
      inf = dist_args$inf,
      symp = dist_args$symp,
      detect = dist_args$detect
    )

    c_test_args$test <- reactive(list(
      p_symp = dist_args$test()$p_symp,
      p_asymp_vac = p_vac,
      p_asymp_unvac = p_unvac
    ))
  } else {
    c_test_args <- dist_args
    c_test_args$test$p_asymp_vac   <- p_vac
    c_test_args$test$p_asymp_unvac <- p_unvac
  }

  c_test_args
}

toggle_panel <- function(
  id,
  ...,
  label = NULL,
  icon = NULL,
  tag_fn = NULL,
  init_visible = FALSE
) {
  panel <- tags$div(id = paste0(id, "_panel"), ...)
  tags$div(
    action_link(id, label = label, icon = icon, tag_fn = tag_fn),
    if (init_visible) panel else shinyjs::hidden(panel)
  )
}

toggle_panel_server <- function(
  id,
  input,
  anim = TRUE,
  animType = "slide",
  time = 0.5
) {
  bindEvent(
    observe(shinyjs::toggle(paste0(id, "_panel"),
                            anim = anim,
                            animType = animType,
                            time = time
    )),
    input[[id]]
  )
}
