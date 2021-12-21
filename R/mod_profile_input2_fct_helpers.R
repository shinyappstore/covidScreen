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
                label = "% Vaccinated (in Organization)")
  )

  # Wrap in column
  column(width = 4, toggle_panel(
    id,
    init_visible = TRUE,
    label = "Organization",
    icon = icon("user-shield"),
    tag_fn = h5,
    inputs
  ))
}
