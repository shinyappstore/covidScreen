#' Show/Hide and Provide Info on an Input Panel
#'
#' `input_panel_server()` provides methods to show/hide an input panel by
#' clicking on the title, as well as get more information by clicking on the
#' info link. The info link should be named `"{id}_info"`.
#'
#' @param id The panel ID (technically the id of the title)
#' @param toggle_time The amount of time (in seconds) it takes to show/hide
#'   the panel
#' @param info A `tagList`, tag, or text to display when info link is clicked
#' @param info_label A label for the info observer; defaults to `"{id}_info()"`
#' @param session The shiny `session` object; the default is usually appropriate
#'
#' @return The output of a `bindEvent` call
#'
#' @keywords internal
input_panel_server <- function(
  id,
  toggle_time,
  info,
  info_label = paste0(id, "_info()"),
  session = getDefaultReactiveDomain()
) {
  toggle_panel_server(id, input = session$input, time = toggle_time)
  bindEvent(
    observe(cs_info_server(info)),
    session$input[[paste0(id, "_info")]],
    label = info_label
  )
}


# Organization input panel show/hide and info link
org_panel_server <- function(id = "org_link", time = 0.5) {
  info <- tagList(
    p(HTML(
      "Organizational inputs directly involve the organization. They are",
      " <b>interventions</b> that can be used to keep COVID-19 under control",
      " in the organization."
    )),
    p(HTML(
      "<b>Organization Size</b> is the number of people in the organization."
    )),
    p(HTML(
      "<b>Unvaccinated Testing</b> is the number of days between asymptomatic",
      " screening tests for unvaccinated people."
    )),
    p(HTML(
      "<b>Vaccinated Testing</b> is the number of days between asymptomatic",
      " screening tests for vaccinated people."
    )),
    p(HTML(
      "<b>% Vaccinated (in Organization)</b> is the percentage of people in",
      " the organization who are fully vaccinated."
    ))
  )
  input_panel_server(id = id, toggle_time = time, info = info)
}


# Community input panel show/hide and info link
comm_panel_server <- function(id = "comm_link", time = 0.3) {
  info <- tagList(
    p(
    "Community inputs define the context in which the organization resides.",
    " They help determine the baseline infection risk of an organization."
    ),
    p(HTML(
      "<b>% Vaccinated (in Community)</b> is the percentage of people in the",
      "surrounding area who are fully vaccinated."
    )),
    p(HTML(
      "<b>Case Rate (per 100k per day)</b> is the number of new COVID-19 cases per day",
      "per 100,000 people in the area. This is likely higher than the reported",
      "case rate; see 'How do I choose the community case rate?' in the FAQ."
    ))
  )
  input_panel_server(id = id, toggle_time = time, info = info)
}


# Advanced input panel show/hide and info link
advanced_panel_server <- function(id = "advanced_link", time = 0.4) {
  info <- p(HTML(
    "Advanced inputs are related to properties of COVID-19, its tests, and",
    " it vaccinations. They are 'advanced' in the sense that the average",
    " user is likely not an expert on COVID-19, and may not know reasonable",
    " values for these inputs. The defaults should suffice for many",
    " settings, but users are encouraged to change the defaults if they do",
    " not match your setting. The <b>Profiling</b> tab allows users to explore",
    " the effects of these parameters (and others) in detail."
  ))
  input_panel_server(id = id, toggle_time = time, info = info)
}


# Vaccine efficacy input panel show/hide and info link
vac_eff_panel_server <- function(id = "vac_eff_link", time = 0.2) {
  info <- p(HTML(
    "<b>Vaccine efficacy</b> is the average amount of risk it mitigates. For example",
    " a vaccine that is 70% effective takes away 70% of the unvaccinated risk",
    " of infection, but 30% of the original risk remains. A 100% effective",
    " vaccine would give complete immunity; a 0% effective one would have no",
    " effect."
  ))
  input_panel_server(id = id, toggle_time = time, info = info)
}


# Testing input panel show/hide and info link
test_panel_server <- function(id = "test_link", time = 0.4) {
  info <- tagList(
    p(
      "Testing inputs define properties of COVID-19 tests and the likelihood of",
      " testing based on symptoms alone."
    ),
    p(HTML(
      "<b>Test Sensitivity (%)</b> is the chance that a person with COVID-19",
      "will actually test positive. See 'How do I choose test sensitivity and",
      "specificity?' for help choosing non-default values."
    )),
    p(HTML(
      "<b>Test Specificity (%)</b> is the chance that a person who does not have",
      "COVID-19 will actually test negative. See 'How do I choose test",
      "sensitivity and specificity?' for help choosing non-default values."
    )),
    p(HTML(
      "<b>% Syptomatics Tested</b> is the percentage of people with COVID-19 symptoms",
      "who are tested based on symptoms alone (not due to screening, travel, or",
      "other reasons)."
    ))
  )
  input_panel_server(id = id, toggle_time = time, info = info)
}


# Symptoms input panel show/hide and info link
symp_panel_server <- function(id = "symp_link", time = 0.6) {
  info <- p(
    "Symptom inputs define how often and for how long infected people have",
    " COVID symptoms. They also define how often people without COVID get",
    " COVID symptoms."
  )
  input_panel_server(id = id, toggle_time = time, info = info)
}
