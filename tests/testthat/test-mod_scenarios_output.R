library(shiny)
dist_args <- reactiveValues(
  n = reactive(2000),
  vac = reactive(list(p_comm = 0.6, p_org = 0.4, eff = 0.5)),
  inf = reactive(list(p_incid = 0.005, t_symp = 7, t_presymp = 3)),
  symp = reactive(list(p_inf_vac = 0.4, p_inf_unvac = 0.6, p_uninf = 0.01)),
  test = reactive(list(p_symp = 0.9, p_asymp_vac = 0, p_asymp_unvac = 1/4)),
  detect = reactive(list(sens = 0.9, spec = 0.99))
)


test_that("scenarios output server plots have not changed", {
  testServer(mod_scenarios_output_server, args = list(dist_args = dist_args), {
    risk_json <- jsonlite::parse_json(output$risk_plot)
    benefit_json <- jsonlite::parse_json(output$benefit_plot)
    vac_json <- jsonlite::parse_json(output$vac_plot)
    expect_snapshot(risk_json$x$hc_opts$series)
    expect_snapshot(benefit_json$x$hc_opts$series)
    expect_snapshot(vac_json$x$hc_opts$series)
  })
})


test_that("scenarios output server text has not changed", {
  testServer(mod_scenarios_output_server, args = list(dist_args = dist_args), {
    expect_snapshot(output$risk_expl)
    expect_snapshot(output$benefit_expl)
    expect_snapshot(output$vac_expl)
  })
})
