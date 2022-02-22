library(shiny)
make_input <- function(id, point, range, trigger = "x_var") {
  # Check args
  checkmate::assert_character(id, min.chars = 1L, any.missing = FALSE, len = 1L)
  checkmate::assert_numeric(point, finite = TRUE, any.missing = FALSE, len = 1L)
  checkmate::assert_numeric(range, finite = TRUE, any.missing = FALSE, len = 2L)
  checkmate::assert_character(trigger, min.chars = 1L, any.missing = FALSE, len = 1L)
  # Create object
  x <- list(point = point, range = range)
  # Add attributes
  attr(x, "id") <- id
  attr(x, "trigger") <- trigger
  # Return
  x
}

n_org_point <- 2000
n_org_range <- c(1000, 3000)
vac_point <- list(p_comm = 60, p_org = 40, eff = 50)
vac_range <- list(p_comm = c(0, 100), p_org = c(0, 100), eff = c(0, 100))
inf_point <- list(r_incid = 500, t_symp = 7, t_presymp = 3)
inf_range <- list(r_incid = c(10, 1000), t_symp = c(1, 9), t_presymp = c(1, 9))
symp_point <- list(p_inf_vac = 40, p_inf_unvac = 60, p_uninf = 1)
symp_range <- list(p_inf_vac = c(0, 100), p_inf_unvac = c(0, 100), p_uninf = c(0, 100))
test_point <- list(p_symp = 90, f_asymp_vac = 0, f_asymp_unvac = 4)
test_range <- list(p_symp = c(0, 100), f_asymp_vac = c(0, 100), f_asymp_unvac = c(0, 100))
detect_point <- list(sens = 90, spec = 99)
detect_range <- list(sens = c(0, 100), spec = c(50, 100))

x_var <- "n_org"
y_var <- "risk"
calc  <- 0L

assign_range <- function(lst, i) {
  x_point <- paste0(lst, "_point")
  x_range <- paste0(lst, "_range")
  i <- paste0("'", i, "'")
  value <- paste0(x_range, "[", i, "]")

  expr <- paste0("vctrs::vec_assign(",
    x_point, ",",
    "i = ", i, ",",
    "value = ", value,
  ")")
  eval(rlang::parse_expr(expr))
}

test_that("profiling input server returns expected outputs", {
  testServer(mod_profiling_input_server, {
    session$setInputs(
      n_org = make_input("n_org", n_org_point, n_org_range),
      vac_p_comm = make_input("vac_p_comm", vac_point$p_comm, vac_range$p_comm),
      vac_p_org = make_input("vac_p_org", vac_point$p_org, vac_range$p_org),
      vac_eff = make_input("vac_eff", vac_point$eff, vac_range$eff),
      inf_r_incid = make_input("inf_r_incid", inf_point$r_incid, inf_range$r_incid),
      inf_t_symp = make_input("inf_t_symp", inf_point$t_symp, inf_range$t_symp),
      inf_t_presymp = make_input("inf_t_presymp", inf_point$t_presymp, inf_range$t_presymp),
      symp_p_inf_vac = make_input("symp_p_inf_vac", symp_point$p_inf_vac, symp_range$p_inf_vac),
      symp_p_inf_unvac = make_input("symp_p_inf_unvac", symp_point$p_inf_unvac, symp_range$p_inf_unvac),
      symp_p_uninf = make_input("symp_p_uninf", symp_point$p_uninf, symp_range$p_uninf),
      test_p_symp = make_input("test_p_symp", test_point$p_symp, test_range$p_symp),
      test_f_asymp_vac = make_input("test_f_asymp_vac", test_point$f_asymp_vac, test_range$f_asymp_vac),
      test_f_asymp_unvac = make_input("test_f_asymp_unvac", test_point$f_asymp_unvac, test_range$f_asymp_unvac),
      detect_sens = make_input("detect_sens", detect_point$sens, detect_range$sens),
      detect_spec = make_input("detect_spec", detect_point$spec, detect_range$spec),
      x_var = x_var,
      y_var = y_var,
      calc = calc
    )

    n_org_rtn <- function() session$returned$n$org()
    vac_rtn <- function() session$returned$dist_args$vac()
    inf_rtn <- function() session$returned$dist_args$inf()
    symp_rtn <- function() session$returned$dist_args$symp()
    test_rtn <- function() session$returned$dist_args$test()
    detect_rtn <- function() session$returned$dist_args$detect()

    # w/ x_var == "n_org"
    expect_equal(n_org_rtn(), n_org_range)
    expect_equal(vac_rtn(), vac_point)
    expect_equal(inf_rtn(), inf_point)
    expect_equal(symp_rtn(), symp_point)
    expect_equal(test_rtn(), test_point)
    expect_equal(detect_rtn(), detect_point)

    # w/ x_var == "vac_p_comm"
    session$setInputs(x_var = "vac_p_comm")
    expect_equal(n_org_rtn(), n_org_point)
    expect_equal(vac_rtn(), assign_range("vac", "p_comm"))
    # w/ x_var == "vac_p_org"
    session$setInputs(x_var = "vac_p_org")
    expect_equal(vac_rtn(), assign_range("vac", "p_org"))
    # w/ x_var == "vac_eff"
    session$setInputs(x_var = "vac_eff")
    expect_equal(vac_rtn(), assign_range("vac", "eff"))

    # w/ x_var == "inf_r_incid"
    session$setInputs(x_var = "inf_r_incid")
    expect_equal(vac_rtn(), vac_point)
    expect_equal(inf_rtn(), assign_range("inf", "r_incid"))
    # w/ x_var == "inf_t_symp"
    session$setInputs(x_var = "inf_t_symp")
    expect_equal(inf_rtn(), assign_range("inf", "t_symp"))
    # w/ x_var == "inf_t_presymp"
    session$setInputs(x_var = "inf_t_presymp")
    expect_equal(inf_rtn(), assign_range("inf", "t_presymp"))

    # w/ x_var == "symp_p_inf_vac"
    session$setInputs(x_var = "symp_p_inf_vac")
    expect_equal(inf_rtn(), inf_point)
    expect_equal(symp_rtn(), assign_range("symp", "p_inf_vac"))
    # w/ x_var == "symp_p_inf_unvac"
    session$setInputs(x_var = "symp_p_inf_unvac")
    expect_equal(symp_rtn(), assign_range("symp", "p_inf_unvac"))
    # w/ x_var == "symp_p_uninf"
    session$setInputs(x_var = "symp_p_uninf")
    expect_equal(symp_rtn(), assign_range("symp", "p_uninf"))

    # w/ x_var == "test_p_symp"
    session$setInputs(x_var = "test_p_symp")
    expect_equal(symp_rtn(), symp_point)
    expect_equal(test_rtn(), assign_range("test", "p_symp"))
    # w/ x_var == "test_f_asymp_vac"
    session$setInputs(x_var = "test_f_asymp_vac")
    expect_equal(test_rtn(), assign_range("test", "f_asymp_vac"))
    # w/ x_var == "test_f_asymp_unvac"
    session$setInputs(x_var = "test_f_asymp_unvac")
    expect_equal(test_rtn(), assign_range("test", "f_asymp_unvac"))

    # w/ x_var == "detect_sens"
    session$setInputs(x_var = "detect_sens")
    expect_equal(test_rtn(), test_point)
    expect_equal(detect_rtn(), assign_range("detect", "sens"))
    session$setInputs(x_var = "detect_spec")
    expect_equal(detect_rtn(), assign_range("detect", "spec"))

    # w/ x_var == "n_org" again
    session$setInputs(x_var = "n_org")
    expect_equal(detect_rtn(), detect_point)
    expect_equal(n_org_rtn(), n_org_range)

    # y_var works (x already checked implicitly)
    session$setInputs(y_var = "risk")
    expect_equal(session$returned$vars$y(), "risk")
    session$setInputs(y_var = "benefit")
    expect_equal(session$returned$vars$y(), "benefit")
    session$setInputs(y_var = "vac")
    expect_equal(session$returned$vars$y(), "vac")
    session$setInputs(y_var = y_var)

    # calc works
    session$setInputs(calc = 0L)
    expect_equal(session$returned$calc(), 0L)
    session$setInputs(calc = 1L)
    expect_equal(session$returned$calc(), 1L)
  })
})
