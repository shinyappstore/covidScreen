output_panel_server_scenarios <- function(
  id,
  info,
  info_label = paste0(id, "_info()"),
  session = getDefaultReactiveDomain()
) {
  toggle_panel_server(id, input = session$input)
  bindEvent(
    observe(cs_info_server(info)),
    session$input[[paste0(id, "_info")]],
    label = info_label
  )
}


risk_panel_server <- function(id = "risk_link") {
  info <- tagList(
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
  output_panel_server_scenarios(id, info = info)
}


benefit_panel_server <- function(id = "benefit_link") {
  info <- tagList(
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
  output_panel_server_scenarios(id, info = info)
}


vac_panel_server <- function(id = "vac_link") {
  info <- tagList(
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
  output_panel_server_scenarios(id, info = info)
}
