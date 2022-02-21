test_that("ct_dist() output has not changed", {
  expect_snapshot(ct_dist())
})


test_that("ct_dist() works with partial args", {
  data <- ct_dist(
    vac = list(p_org = 0.7),
    inf = list(t_symp = 7),
    symp = list(p_uninf = 0.01),
    test = list(p_asymp_vac = 1/4),
    detect = list(spec = 0.97)
  )
  is_internal_selfref <- which(names(attributes(data)) == ".internal.selfref")
  attrs <- attributes(data)[-is_internal_selfref]
  expect_snapshot(data)
  expect_snapshot(attrs)
})


test_that("ct_dist() works with no testing", {
  data <- ct_dist(test = list(p_symp = 0, p_asymp_vac = 0, p_asymp_unvac = 0))
  expect_equal(sum(data$p[data$test]), 0)
  expect_equal(sum(data$p[data$detect]), 0)
})


test_that("reactive_dist() works", {
  args <- lapply(rlang::fn_fmls(calc_dist), eval)

  reactive_args <- list(
    vac = reactive(args$vac),
    inf = reactive(args$inf),
    symp = reactive(args$symp),
    test = reactive(args$test),
    detect = reactive(args$detect)
  )

  reactive_dt <- isolate(reactive_dist(reactive_args)()[])
  expected_dt <- calc_dist()[]

  expect_equal(reactive_dt, expected_dt)
})
