#' Risk-Based Metrics
#'
#' @description
#' Undetected cases are the primary measure of risk in the model. From them,
#' one can calculate the risk reduction and the cost effectiveness of testing.
#'
#' \code{cs_undetected()} calculates the proportion of undetected cases
#' \code{cs_rr()} calculates the reduction in risk relative to no screening
#' \code{cs_cost_eff()} calculates the number of cases detected per test
#'
#' @param dt \code{[data.table]} The joint distribution from a \code{cs_dist()}
#' @param relative \code{[logical(1)]} Whether to return risk reduction relative
#'   to baseline risk (\code{TRUE}) or as an absolute proportion of the
#'   organization (\code{FALSE})
#'
#' @return \code{[double(1)]} A proportion
#'
#' @name risk-metrics
NULL

#' @rdname risk-metrics
#' @export
cs_undetected <- function(dt) {
  cs_sum(dt, inf = TRUE, detect = FALSE)
}


#' @rdname risk-metrics
#' @export
cs_rr <- function(dt, relative = TRUE) {
  checkmate::assert_logical(relative, any.missing = FALSE, len = 1)
  p0 <- const_testing(attr(dt, "params", exact = TRUE), p_vac = 0, p_unvac = 0)
  u0 <- undetected(do.call(calc_dist, p0))
  reduction <- u0 - undetected(dt)
  if (relative) reduction / u0 else reduction
}

#' @rdname risk-metrics
#' @export
cs_cost_eff <- function(dt) {
  cs_true_pos(dt) / sum(dt$p[dt$test])
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
#' \code{cs_pos()} is the proportion of positive tests (out of the organization)
#' \code{cs_neg()} is the proportion of negative tests (out of the organization)
#' \code{cs_true_pos()} is the proportion of true positive tests (out of org)
#' \code{cs_true_neg()} is the proportion of true negative tests (out of org)
#' \code{cs_false_pos()} is the proportion of false positive tests (out of org)
#' \code{cs_false_neg()} is the proportion of false negative tests (out of org)
#' \code{cs_ppv()} is the positive predictive value of a test
#' \code{cs_npv()} is the negative predictive value of a test
#' \code{cs_fdr()} is the false discovery rate of a test
#' \code{cs_for()} is the false omission rate of a test
#' \code{cs_sens()} is the sensitivity (true positive rate) of a test
#' \code{cs_spec()} is the specificity (true negative rate) of a test
#' \code{cs_fpr()} is the false positive rate of a test
#' \code{cs_fnr()} is the false negative rate of a test
#'
#' @param dt \code{[data.table]} A distribution from \code{cs_dist()}
#'
#' @return \code{[numeric]} The specified metric
#'
#' @name test-metrics
NULL


#' @rdname test-metrics
#' @export
cs_pos <- function(dt) {
  sum(dt$p[dt$detect])
}


#' @rdname test-metrics
#' @export
cs_neg <- function(dt) {
  sum(dt$p[dt$test & !dt$detect])
}


#' @rdname test-metrics
#' @export
cs_true_pos <- function(dt) {
  sum(dt$p[dt$inf & dt$detect])
}


#' @rdname test-metrics
#' @export
cs_false_pos <- function(dt) {
  sum(dt$p[!dt$inf & dt$detect])
}


#' @rdname test-metrics
#' @export
cs_true_neg <- function(dt) {
  sum(dt$p[!dt$inf & dt$test & !dt$detect])
}


#' @rdname test-metrics
#' @export
cs_false_neg <- function(dt) {
  sum(dt$p[dt$inf & dt$test & !dt$detect])
}


#' @rdname test-metrics
#' @export
cs_ppv <- function(dt) {
  cs_true_pos(dt) / cs_pos(dt)
}


#' @rdname test-metrics
#' @export
cs_npv <- function(dt) {
  cs_true_neg(dt) / cs_neg(dt)
}


#' @rdname test-metrics
#' @export
cs_fdr <- function(dt) {
  1 - cs_ppv(dt)
}


#' @rdname test-metrics
#' @export
cs_for <- function(dt) {
  1 - cs_npv(dt)
}


#' @rdname test-metrics
#' @export
cs_sens <- function(dt) {
  attr(dt, "params", exact = TRUE)$detect$sens
}


#' @rdname test-metrics
#' @export
cs_spec <- function(dt) {
  attr(dt, "params", exact = TRUE)$detect$spec
}


#' @rdname test-metrics
#' @export
cs_fnr <- function(dt) {
  1 - cs_sens(dt)
}


#' @rdname test-metrics
#' @export
cs_fpr <- function(dt) {
  1 - cs_spec(dt)
}


#' Summarize Joint Distribution from \code{cs_dist()}
#'
#' \code{cs_sum()} calculates the proportion of the organization
#' that falls within the specified group. Variables are joined
#' with \code{&}; not specifiying a variable (or setting it to \code{NULL})
#' includes all of its values.
#'
#' @param dt \code{[data.table]} A distribution from \code{cs_dist()}
#' @param vac,inf,symp,test,detect \code{[logical(1)]} Variables values specifying
#'   which sub-group to sum over. If \code{NULL}, will sum over all values.
#'
#' @return \code{[double(1)]} The proportion of the organization with all the specified
#'   characteristics
#'
#' @export
cs_sum <- function(
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


cs_subset <- function(
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

  dt[v & i & s & t & d]
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
