## code to prepare `DATASET` dataset goes here

profile_arg_labels <- list(
  n = list(org = "Organization Size (People)"),
  vac = list(
    p_comm = "% Vaccinated (in Community)",
    p_org  = "% Vaccinated (in Organization)",
    eff    = "Vaccine Efficacy (%)"
  ),
  inf = list(
    r_incid   = "Case Rate (per 100k per day)",
    t_symp    = "Symptomatic Period (Days)",
    t_presymp = "Pre-Symptomatic Period (Days)"
  ),
  symp = list(
    p_inf_unvac = "% Symptomatic: Unvaccinated Cases",
    p_inf_vac   = "% Symptomatic: Vaccinated Cases",
    p_uninf     = "% Symptomatic: Non-Cases"
  ),
  test = list(
    p_symp = "% Symptomatics Tested",
    p_asymp_vac = "Vaccinated Testing (Days)",
    p_asymp_unvac = "Unvaccinated Testing (Days)"
  ),
  detect = list(
    sens = "Test Sensitivity (%)",
    spec = "Test Specificity (%)"
  )
)

usethis::use_data(profile_arg_labels, overwrite = TRUE, internal = TRUE)
