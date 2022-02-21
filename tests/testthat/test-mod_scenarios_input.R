library(shiny)
n_org_in <- 2000
vac <- list(p_comm = 0.6, p_org = 0.4, eff = 0.5)
inf <- list(p_incid = 0.005, t_symp = 7, t_presymp = 3)
symp <- list(p_inf_vac = 0.4, p_inf_unvac = 0.6, p_uninf = 0.01)
test <- list(p_symp = 0.9, p_asymp_vac = 0, p_asymp_unvac = 1/4)
detect <- list(sens = 0.9, spec = 0.99)

test_that("scenario input server is invertible", {
  testServer(mod_scenarios_input_server, {
    # Initialize inputs
    session$setInputs(
      n_org = n_org_in,
      test_f_asymp_unvac = if (test$p_asymp_unvac == 0) 0 else 1 / test$p_asymp_unvac,
      test_f_asymp_vac = if (test$p_asymp_vac == 0) 0 else 1 / test$p_asymp_vac,
      vac_p_org = vac$p_org * 100,
      inf_r_incid = inf$p_incid * 1e5,
      vac_p_comm = vac$p_comm * 100,
      vac_eff = vac$eff * 100,
      detect_sens = detect$sens * 100,
      detect_spec = detect$spec * 100,
      test_p_symp = test$p_symp * 100,
      inf_t_symp = inf$t_symp,
      inf_t_presymp = inf$t_presymp,
      symp_p_inf_unvac = symp$p_inf_unvac * 100,
      symp_p_inf_vac = symp$p_inf_vac * 100,
      symp_p_uninf = symp$p_uninf * 100
    )

    # Get return values
    n_rtn   <- session$returned$n()
    vac_rtn <- session$returned$vac()
    inf_rtn <- session$returned$inf()
    symp_rtn <- session$returned$symp()
    test_rtn <- session$returned$test()
    detect_rtn <- session$returned$detect()

    # Compare to initial values
    expect_equal(n_rtn, n_org_in)
    expect_equal(vac_rtn, vac)
    expect_equal(inf_rtn, inf)
    expect_equal(symp_rtn, symp)
    expect_equal(test_rtn, test)
    expect_equal(detect_rtn, detect)
  })
})


test_that("scenario input server cancels invalid inputs", {
  testServer(mod_scenarios_input_server, {
    # Initialize inputs
    session$setInputs(
      n_org = n_org_in,
      test_f_asymp_unvac = if (test$p_asymp_unvac == 0) 0 else 1 / test$p_asymp_unvac,
      test_f_asymp_vac = if (test$p_asymp_vac == 0) 0 else 1 / test$p_asymp_vac,
      vac_p_org = vac$p_org * 100,
      inf_r_incid = inf$p_incid * 1e5,
      vac_p_comm = vac$p_comm * 100,
      vac_eff = vac$eff * 100,
      detect_sens = detect$sens * 100,
      detect_spec = detect$spec * 100,
      test_p_symp = test$p_symp * 100,
      inf_t_symp = inf$t_symp,
      inf_t_presymp = inf$t_presymp,
      symp_p_inf_unvac = symp$p_inf_unvac * 100,
      symp_p_inf_vac = symp$p_inf_vac * 100,
      symp_p_uninf = symp$p_uninf * 100
    )

    # Make org size invalid
    session$setInputs(n_org = -1)
    expect_error(session$returned$n(), class = "shiny.output.cancel")
    expect_error(session$returned$n(), class = "shiny.silent.error")
    session$setInputs(n_org = n_org_in)

    # Make asymp unvac testing frequency invalid
    session$setInputs(test_f_asymp_unvac = -1)
    expect_error(session$returned$test(), class = "shiny.output.cancel")
    expect_error(session$returned$test(), class = "shiny.silent.error")
    session$setInputs(
      test_f_asymp_unvac = if (test$p_asymp_unvac == 0) 0 else 1 / test$p_asymp_unvac
    )
    # Make asymp vac testing frequency invalid
    session$setInputs(test_f_asymp_vac = -1)
    expect_error(session$returned$test(), class = "shiny.output.cancel")
    expect_error(session$returned$test(), class = "shiny.silent.error")
    session$setInputs(
      test_f_asymp_vac = if (test$p_asymp_vac == 0) 0 else 1 / test$p_asymp_vac
    )
    # Make incidence invalid
    session$setInputs(inf_r_incid = -1)
    expect_error(session$returned$inf(), class = "shiny.output.cancel")
    expect_error(session$returned$inf(), class = "shiny.silent.error")
    session$setInputs(inf_r_incid = inf$p_incid * 1e5)
    # Make symptomatic period invalid
    session$setInputs(inf_t_symp = -1)
    expect_error(session$returned$inf(), class = "shiny.output.cancel")
    expect_error(session$returned$inf(), class = "shiny.silent.error")
    session$setInputs(inf_t_symp = inf$t_symp)
    # Make pre-symptomatic period invalid
    session$setInputs(inf_t_presymp = -1)
    expect_error(session$returned$inf(), class = "shiny.output.cancel")
    expect_error(session$returned$inf(), class = "shiny.silent.error")
    session$setInputs(inf_t_presymp = inf$t_presymp)
    # Ensure no error were left uncorrected
    expect_equal(session$returned$n(), n_org_in)
    expect_equal(session$returned$inf(), inf)
    expect_equal(session$returned$test(), test)
  })
})
