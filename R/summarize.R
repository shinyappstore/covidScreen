#' Risk-Based Metrics
#'
#' @description
#' Undetected cases are the primary measure of risk in the model. From them,
#' one can calculate the risk reduction and the cost effectiveness of testing.
#'
#' `ct_undetected()` calculates the proportion of undetected cases
#' `ct_rr()` calculates the reduction in risk relative to no screening
#' `ct_cost_eff()` calculates the number of cases detected per test
#'
#' @param dt `[data.table]` The joint distribution from a `ct_dist()`
#' @param relative `[logical(1)]` Whether to return risk reduction relative
#'   to baseline risk (`TRUE`) or as an absolute proportion of the organization
#'   (`FALSE`)
#'
#' @return `[double]` A proportion
#'
#' @name risk-metrics
NULL

#' @rdname risk-metrics
#' @export
ct_undetected <- function(dt) {
  ct_sum(dt, inf = TRUE, detect = FALSE)
}


#' @rdname risk-metrics
#' @export
ct_rr <- function(dt, relative = TRUE) {
  checkmate::assert_logical(relative, any.missing = FALSE, len = 1)
  p0 <- const_testing(attr(dt, "params", exact = TRUE), p_vac = 0, p_unvac = 0)
  u0 <- undetected(do.call(calc_dist, p0))
  reduction <- u0 - undetected(dt)
  if (relative) reduction / u0 else reduction
}

#' @rdname risk-metrics
#' @export
ct_cost_eff <- function(dt) {
  ct_true_pos(dt) / sum(dt$p[dt$test])
}


#' Testing Metrics
#'
#' @description
#' There are a number of metrics available to evaluate test performance.
#' These are the basics, from which one can calculate other metrics. See the
#' Wikipedia page on the
#' \href{https://en.wikipedia.org/wiki/Confusion_matrix#Table_of_confusion}{confusion matrix}
#' for more information on each metric.
#'
#' `ct_pos()` is the proportion of positive tests (out of the organization)
#' `ct_neg()` is the proportion of negative tests (out of the organization)
#' `ct_true_pos()` is the proportion of true positive tests (out of org)
#' `ct_true_neg()` is the proportion of true negative tests (out of org)
#' `ct_false_pos()` is the proportion of false positive tests (out of org)
#' `ct_false_neg()` is the proportion of false negative tests (out of org)
#' `ct_ppv()` is the positive predictive value of a test
#' `ct_npv()` is the negative predictive value of a test
#' `ct_fdr()` is the false discovery rate of a test
#' `ct_for()` is the false omission rate of a test
#' `ct_sens()` is the sensitivity (true positive rate) of a test
#' `ct_spec()` is the specificity (true negative rate) of a test
#' `ct_fpr()` is the false positive rate of a test
#' `ct_fnr()` is the false negative rate of a test
#'
#' @param dt `[data.table]` A distribution from `ct_dist()`
#'
#' @return `[numeric]` The specified metric
#'
#' @name test-metrics
NULL


#' @rdname test-metrics
#' @export
ct_pos <- function(dt) {
  sum(dt$p[dt$detect])
}


#' @rdname test-metrics
#' @export
ct_neg <- function(dt) {
  sum(dt$p[dt$test & !dt$detect])
}


#' @rdname test-metrics
#' @export
ct_true_pos <- function(dt) {
  sum(dt$p[dt$inf & dt$detect])
}


#' @rdname test-metrics
#' @export
ct_false_pos <- function(dt) {
  sum(dt$p[!dt$inf & dt$detect])
}


#' @rdname test-metrics
#' @export
ct_true_neg <- function(dt) {
  sum(dt$p[!dt$inf & dt$test & !dt$detect])
}


#' @rdname test-metrics
#' @export
ct_false_neg <- function(dt) {
  sum(dt$p[dt$inf & dt$test & !dt$detect])
}


#' @rdname test-metrics
#' @export
ct_ppv <- function(dt) {
  ct_true_pos(dt) / ct_pos(dt)
}


#' @rdname test-metrics
#' @export
ct_npv <- function(dt) {
  ct_true_neg(dt) / ct_neg(dt)
}


#' @rdname test-metrics
#' @export
ct_fdr <- function(dt) {
  1 - ct_ppv(dt)
}


#' @rdname test-metrics
#' @export
ct_for <- function(dt) {
  1 - ct_npv(dt)
}


#' @rdname test-metrics
#' @export
ct_sens <- function(dt) {
  attr(dt, "params", exact = TRUE)$detect$sens
}


#' @rdname test-metrics
#' @export
ct_spec <- function(dt) {
  attr(dt, "params", exact = TRUE)$detect$spec
}


#' @rdname test-metrics
#' @export
ct_fnr <- function(dt) {
  1 - ct_sens(dt)
}


#' @rdname test-metrics
#' @export
ct_fpr <- function(dt) {
  1 - ct_spec(dt)
}


#' Summarize Joint Distribution from `ct_dist()`
#'
#' `ct_sum()` calculates the proportion of the organization
#' that falls within the specified group. Variables are joined
#' with `and`; not specifiying a variable (or setting it to `NULL`)
#' includes all of its values.
#'
#' @param dt `[data.table]` A distribution from `ct_dist()`
#' @param vac,inf,symp,test,detect `[logical(1)]` Variables values specifying
#'   which sub-group to sum over. If `NULL`, will sum over all values.
#'
#' @return `[double]` The proportion of the organization with all the specified
#'   characteristics
#'
#' @export
ct_sum <- function(
  dt,
  vac = NULL,
  inf = NULL,
  symp = NULL,
  test = NULL,
  detect = NULL
) {
  assert_summary_filters(
    vac = vac,
    inf = inf,
    symp = symp,
    test = test,
    detect = detect
  )
  v <- if (is.null(vac) || is.na(vac)) TRUE else dt$vac == vac
  i <- if (is.null(inf) || is.na(inf)) TRUE else dt$inf == inf
  s <- if (is.null(symp) || is.na(symp)) TRUE else dt$symp == symp
  t <- if (is.null(test) || is.na(test)) TRUE else dt$test == test
  d <- if (is.null(detect) || is.na(detect)) TRUE else dt$detect == detect

  sum(dt$p[v & i & s & t & d])
}


assert_summary_filters <- function(
  vac = NULL,
  inf = NULL,
  symp = NULL,
  test = NULL,
  detect = NULL,
  negate = FALSE
) {
  checkmate::assert_logical(vac, any.missing = TRUE, len = 1, null.ok = TRUE)
  checkmate::assert_logical(inf, any.missing = TRUE, len = 1, null.ok = TRUE)
  checkmate::assert_logical(symp, any.missing = TRUE, len = 1, null.ok = TRUE)
  checkmate::assert_logical(test, any.missing = TRUE, len = 1, null.ok = TRUE)
  checkmate::assert_logical(detect, any.missing = TRUE, len = 1, null.ok = TRUE)
  checkmate::assert_logical(
    negate,
    any.missing = FALSE,
    len = 1,
    null.ok = FALSE
  )
}
