#' home UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_ui <- function(id){
  ns <- NS(id)
  tagList(
    h2("COVID-19 Testing Strategy Assessment for Organizations"),
    tags$p(
      "As the COVID-19 pandemic evolves, organizations are repeatedly faced",
      "with the need to make rapid decisions about the level of acceptable",
      "risk to their people and how best to mitigate that risk.",
      "One mitigation strategy is asymptomatic testing, which tests",
      "organization members (i.e. employees) for COVID-19 at regular intervals.",
      "This facilitates quick identification and isolation of cases within an",
      "organization, limiting the risk of spreading COVID-19 to others. Such",
      "testing is most effective alongside other strategies, such as physical",
      "distancing and vaccination. The tools provided here help decision",
      "makers assess the benefits of testing in their specific organization",
      "across a range of scenarios and parameters."
    ),
    h3("Scenarios"),
    tags$p(
      "The",tags$b("Scenarios"),
      "tab allows users to customize a simulation using",
      "settings that reflect their organizational context. The results of this",
      "simulation are summarized into organizational risk, testing benefit,",
      "and cost-effectiveness by vaccination. This interface is designed to",
      "provide quick answers to 'what-if' questions."
    ),
    h3("Profiling"),
    tags$p(
      "The", tags$b("Profiling"), "tab allows users to explore the effects of",
      "changing a specific setting while keeping others constant. This runs a",
      "series of simulations to show results across a range of values for that",
      "setting. The interface is meant to allow users to dig deeper into",
      "particular scenarios and gain an intuition of how the settings influence",
      "results."
    ),
    h3("How to Use this App"),
    tags$p(
      "Each tab presents the user with a variety of settings to customize.",
      "The most important of these are organization and community settings.",
      tags$b("Organization"), "settings specifies testing frequency, as well",
      "as the organizational context in which tests are performed.",
      tags$b("Community"), "settings specify the case and vaccination rates,",
      "which determine the amount of risk the organization can be exposed to.",
      "These two groups of settings are crucial to tailoring the simulation to",
      "a specific context; the defaults probably do not work for your",
      "organization."
    ),
    tags$p(
      "In addition,", tags$b("Advanced"), "settings allow users to explore",
      "results when using COVID-19 tests with different properties or when",
      "COVID-19 itself evolves different characteristics. Unlike the other",
      "groups, these settings are meant to have sensible defaults and should",
      "provide decent information without much tuning."
    )
  )
}

#' home Server Functions
#'
#' @noRd
mod_home_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_home_ui("home_1")

## To be copied in the server
# mod_home_server("home_1")
