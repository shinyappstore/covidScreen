#' faq UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_faq_ui <- function(id){
  ns <- NS(id)
  tagList(
    h5("How do I set the organization vaccination rate?"),
    tags$p(
      "Some organizations may keep track of employee vaccinations; however,",
      "many will not have a precise estimate. A good starting point is the",
      "vaccination rate reported for the area in which employees reside; this",
      "can be tuned up or down based on whether employees might be more or less",
      "likely than the surrounding population. One can also use the 'Profiling'",
      "tab to explore a range of vaccination rates."
    ),
    h5("How do I set the community vaccination rate?"),
    tags$p(
      "In setting the vaccination rate, you will need to decide what qualifies",
      "as 'vaccinated' (i.e. at least one dose, 'fully vaccinated',",
      "booster shot, etc.). Data on these different levels of vaccination can",
      "often be found via a quick Google search of
      '{geographic area} vaccination rates', or on the local, regional, or",
      "national health organization site. You may need to adjust vaccine",
      "efficacy in the 'Advanced' settings to match your choice of vaccination",
      "level (see 'How do I set vaccine efficacy?'"
    ),
    h5("How do I set community case rates?"),
    tags$p(
      "Case rates in this app are the number of cases per day per 100,000",
      "people in an area. Like vaccination rate, these can often be found via",
      "a quick Google search or browsing of health department websites.",
      "However, the official case rate is almost always less than the actual",
      "number of cases in the community because some people with COVID-19 will",
      "never get tested (this is the same reason we need asymptomatic",
      "screening). A good rule of thumb to be consistent with the default",
      "'% cases symptomatic' setting is to 2x the reported cases. If you",
      "change this setting, you can also change your multiplier to",
      "(1 / proportion symptomatic) x reported cases."
    ),
    h5("How do I set vaccine efficacy?"),
    tags$p(
      "Vaccine efficacy here refers to the ability of the vaccine to reduce the",
      "risk of transmissible COVID-19 infection. It does not refer to protection",
      "against severe disease, which is usually much higher than protection against",
      "infection. Efficacy depends on many different things, including which",
      "vaccine was given, how many doses were given, how long ago it was given,",
      "and which COVID-19 variant is being protected against. At the time of",
      "writing, vaccine efficacy for 'fully vaccinated' individuals is probably",
      "only around 30% against the Omicron variant; however, boosters increase",
      "vaccine efficacy to 70+%. Partial vaccination appears to confer little",
      "protection against Omicron."
    ),
    h5("Can I save my scenario settings so that I don't need to re-input them",
      "again later?"),
    tags$p("Not at the moment, but we hope to add this functionality in the near future."),
    h5("How do I report an issue?"),
    tags$p("You can file an issue in the Github project at",
           tags$a(
             href="https://github.com/jesse-smith/covidscreen/issues",
             "https://github.com/jesse-smith/covidscreen/issues"
            )
    )
  )
}

#' faq Server Functions
#'
#' @noRd
mod_faq_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_faq_ui("faq_1")

## To be copied in the server
# mod_faq_server("faq_1")
