# Organization column definition
org_profiling_column <- function(id, ns) {
  inputs <- tagList(
    # People in organization
    num_input2(ns("n_org"),
               label = "Organization Size (People)",
               value = 1e3,
               value2 = c(1e2, 1e3)),
    # Unvaccinated testing frequency
    num_input2(ns("test_f_asymp_unvac"),
               label = "Unvaccinated Testing (Days)",
               value = 7,
               value2 = c(1, 7)),
    # Vaccinated testing frequency
    num_input2(ns("test_f_asymp_vac"),
               label = "Vaccinated Testing (Days)",
               value = 0,
               value2 = c(1, 7)),
    # % Organization vaccinated
    slider_pct2(ns("vac_p_org"),
                label = "% Vaccinated (in Organization)",
                br = FALSE)
  )

  # Wrap in column and `toggle_panel()`
  column(width = 4, toggle_panel_ui(
    id,
    init_visible = TRUE,
    label = "Organization",
    icon = icon("user-shield"),
    tag_fn = h5,
    inputs
  ))
}


# Community column definition
comm_profiling_column <- function(id, ns) {
  inputs <- tagList(
    # Case rate (incidence)
    num_input2(ns("inf_r_incid"),
               label = "Cases per 100k per Day",
               value = 250,
               value2 = c(100, 1000)),
    # % Vaccinated in community
    slider_pct2(ns("vac_p_comm"),
                label = "% Vaccinated (in Community)",
                br = FALSE)
  )

  # Wrap in column and `toggle_panel()`
  column(width = 4, toggle_panel_ui(
    id,
    init_visible = TRUE,
    label = "Community",
    icon = icon("users"),
    tag_fn = h5,
    inputs
  ))
}


# Advanced column definition
advanced_profiling_column <- function(id, ns) {
  inputs <- tagList(
    # Vaccine effiacy
    vac_eff_panel_ui_profiling(ns("vac_eff_link"), ns = ns),
    # Testing
    test_panel_ui_profiling(ns("test_link"), ns = ns),
    # Symptoms
    symp_panel_ui_profiling(ns("symp_link"), ns = ns)
  )

  # Wrap in column and `toggle_panel()`
  column(width = 4, toggle_panel_ui(
    id,
    init_visible = TRUE,
    label = "Advanced",
    icon = icon("cog"),
    tag_fn = h5,
    inputs
  ))
}


# Vaccine efficacy panel definition
vac_eff_panel_ui_profiling <- function(id, ns) {
  inputs <- tagList(
    # Vaccine efficacy
    slider_pct2(ns("vac_eff"),
                label = NULL,
                value = 30,
                value2 = c(10, 70))
  )

  # Wrap in toggle_panel()
  toggle_panel_ui(
    id,
    init_visible = FALSE,
    label = "Vaccine Efficacy (%)",
    icon = icon("syringe"),
    tag_fn = h6,
    inputs
  )
}


# Testing panel definition
test_panel_ui_profiling <- function(id, ns) {
  inputs <- tagList(
    # Test sensitivity
    slider_pct2(ns("detect_sens"),
                label = "Test Sensitivity (%)",
                value = 85,
                value2 = c(50, 100)),
    # Test specificity
    slider_pct2(ns("detect_spec"),
                label = "Test Specificity (%)",
                value = 99.5,
                value2 = c(90, 100),
                step = 0.1),
    # Symptomatic testing
    slider_pct2(ns("test_p_symp"),
                label = "Symptomatic Tested (%)",
                value = 95,
                value2 = c(50, 100))
  )

  # Wrap in `toggle_panel()`
  toggle_panel_ui(
    id,
    init_visible = FALSE,
    label = "Testing",
    icon = icon("vial"),
    tag_fn = h6,
    inputs
  )
}


# Symptoms panel definition
symp_panel_ui_profiling <- function(id, ns) {
  inputs <- tagList(
    # Symptomatic period
    num_input2(ns("inf_t_symp"),
               label = "Symptomatic Period (Days)",
               value = 10,
               value2 = c(7, 14)
    ),
    # Pre-symptomatic period
    num_input2(ns("inf_t_presymp"),
               label = "Pre-Symptomatic Period (Days)",
               value = 3,
               value2 = c(1, 5)
    ),
    # % of unvaccinated cases that are symptomatic
    slider_pct2(ns("symp_p_inf_unvac"),
                label = "% Symptomatic: Unvaccinated Cases",
                value2 = c(25, 75)
    ),
    # % of vaccinated cases that are symptomatic
    slider_pct2(ns("symp_p_inf_vac"),
                label = "% Symptomatic: Vaccinated Cases",
                value = 30,
                value2 = c(20, 50)
    ),
    # % of non-cases that are symptomatic
    slider_pct2(ns("symp_p_uninf"),
                label = "% Symptomatic: Non-Cases",
                value = 2,
                value2 = c(0, 10),
                step  = 0.1,
                br = FALSE
    )
  )

  # Wrap in `toggle_panel()`
  toggle_panel_ui(
    id,
    init_visible = FALSE,
    label = "Symptoms",
    icon = icon("head-side-cough"),
    tag_fn = h6,
    inputs
  )
}
