# Toggle and info for risk output
risk_panel_ui <- function(id, ns) {
  outputs <- tagList(
    # Risk plot - dots
    column(width = 6, highchartOutput(ns("risk_plot"))),
    # Risk text explanation
    column(width = 6, uiOutput(ns("risk_expl")))
  )

  # Wrap in `fluidRow()` and `toggle_panel_ui()`
  toggle_panel_ui(
    id,
    init_visible = TRUE,
    label = "Undetected Cases (Risk)",
    tag_fn = h3,
    fluidRow(outputs)
  )
}


# Toggle and info for benefit output
benefit_panel_ui <- function(id, ns) {
  outputs <- tagList(
    # Benefit plot - stacked bar
    column(width = 6, highchartOutput(ns("benefit_plot"))),
    # Benefit text explanation
    column(width = 6, uiOutput(ns("benefit_expl")))
  )

  # Wrap in `fluidRow()` and `toggle_panel_ui()`
  toggle_panel_ui(
    id,
    init_visible = TRUE,
    label = "Detected Cases (Benefit)",
    tag_fn = h3,
    fluidRow(outputs)
  )
}


# Toggle and info for vac comparison output
vac_panel_ui <- function(id, ns) {
  outputs <- tagList(
    # Benefits in vac groups - bar chart
    column(width = 6, highchartOutput(ns("vac_plot"))),
    # Explanation text
    column(width = 6, uiOutput(ns("vac_expl")))
  )

  # Wrap in `fluidRow()` and `toggle_panel_ui()`
  toggle_panel_ui(
    id,
    init_visible = TRUE,
    label = "Detection by Vaccination",
    tag_fn = h3,
    fluidRow(outputs)
  )
}
