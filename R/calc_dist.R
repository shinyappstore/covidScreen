# Export -----------------------------------------------------------------------


#' Calculate the Steady-State Joint Distribution for a Population
#'
#' `calc_dist()` calculates the discrete joint distribution of vaccination,
#' infection, symptoms, tests, and detections in a population.
#'
#' @param vac `[list(3)]` A named list containing vaccination parameters:
#'   \describe{
#'     \item{p_comm `[numeric(1)]`}{Proportion vaccinated in the community}
#'     \item{p_org `[numeric(1)]`}{Proportion vaccinated in the organization of interest}
#'     \item{eff `[numeric(1)]`}{Vaccine efficacy}
#'   }
#'
#' @param inf `[list(3)]` A named list containing infection parameters:
#'   \describe{
#'     \item{p_incid `[numeric(1)]`}{Proportion of community newly infected each day}
#'     \item{t_symp `[numeric(1)]`}{Duration of symptomatic period}
#'     \item{t_presymp `[numeric(1)]`}{Duration of presymptomatic period}
#'   }
#'
#' @param symp `[list(3)]` A named lust containing symptom parameters:
#'   \describe{
#'     \item{p_inf_vac}{Proportion of vaccinated infections who are symptomatic}
#'     \item{p_inf_unvac}{Proportion of unvaccinated infections who are symptomatic}
#'     \item{p_uninf}{Proportion of uninfected people who are symptomatic}
#'   }
#'
#' @param test `[list(3)]` A named list containing testing parameters:
#'   \describe{
#'     \item{p_symp `[numeric(1)]`}{Probability of being tested if symptomatic}
#'     \item{p_asymp_vac `[numeric(1)]`}{Probability of being tested if asymptomatic and vaccinated}
#'     \item{p_asymp_unvac `[numeric(1)]`}{Probability of being tested if asymptomatic and unvaccinated}
#'   }
#'
#' @param detect `[list(2)]` A named list containing detection parameters:
#'   \describe{
#'     \item{sens `[numeric(1)]`}{Test sensitivity}
#'     \item{spec `[numeric(1)]`}{Test specificity}
#'   }
#'
#' @return A `data.table`
#'
#' @export
ct_dist <- function(
  # Vaccination parameters
  vac = list(p_comm = 0.5, p_org = 0.5, eff = 0.5),
  # Infection parameters
  inf = list(p_incid = 5e-3, t_symp = 5, t_presymp = 5),
  # Symptom parameters
  symp   = list(p_inf_vac = 0.5, p_inf_unvac = 0.5, p_uninf = 0),
  # Test parameters
  test   = list(p_symp = 1, p_asymp_vac = 0, p_asymp_unvac = 1/7),
  # Detection parameters
  detect = list(sens = 0.85, spec = 1)
) {
  # Get default arguments
  fmls <- purrr::map(rlang::fn_fmls(), eval)

  # Check param structure
  assert_param(vac, fmls$vac)
  assert_param(inf, fmls$inf)
  assert_param(symp, fmls$symp)
  assert_param(test, fmls$test)
  assert_param(detect, fmls$detect)

  # Check `vac`
  v_nms <- names(vac)
  if ("p_comm" %in% v_nms) assert_scalar_prob(vac$p_comm)
  if ("p_org"  %in% v_nms) assert_scalar_prob(vac$p_org)
  if ("eff"    %in% v_nms) assert_scalar_prob(vac$eff)

  # Check `inf`
  i_nms <- names(inf)
  if ("p_incid"   %in% i_nms) assert_scalar_prob(inf$p_incid)
  if ("t_symp"    %in% i_nms) assert_scalar_num(inf$t_symp,    lower = 0)
  if ("t_presymp" %in% i_nms) assert_scalar_num(inf$t_presymp, lower = 0)

  # Check `symp`
  s_nms <- names(symp)
  if ("p_inf_vac"   %in% s_nms) assert_scalar_prob(symp$p_inf_vac)
  if ("p_inf_unvac" %in% s_nms) assert_scalar_prob(symp$p_inf_unvac)
  if ("p_uninf"     %in% s_nms) assert_scalar_prob(symp$p_uninf)

  # Check `test`
  t_nms <- names(test)
  if ("p_symp"        %in% t_nms) assert_scalar_prob(test$p_symp)
  if ("p_asymp_vac"   %in% t_nms) assert_scalar_prob(test$p_asymp_vac)
  if ("p_asymp_unvac" %in% t_nms) assert_scalar_prob(test$p_asymp_unvac)

  # Check `detect`
  d_nms <- names(detect)
  if ("sens" %in% d_nms) assert_scalar_prob(detect$sens)
  if ("spec" %in% d_nms) assert_scalar_prob(detect$spec)

  # Fill in defaults
  v <- c(fmls$vac[!names(fmls$vac) %in% v_nms], vac)
  i <- c(fmls$inf[!names(fmls$inf) %in% i_nms], inf)
  s <- c(fmls$symp[!names(fmls$symp) %in% s_nms], symp)
  t <- c(fmls$test[!names(fmls$test) %in% t_nms], test)
  d <- c(fmls$detect[!names(fmls$detect) %in% d_nms], detect)

  # Calculate distribution
  calc_dist(vac = v, inf = i, symp = s, test = t, detect = d)[]
}


#' Reactive Version of `calc_dist()`
#'
#' Takes a `reactiveValues` object and returns a `reactive`
#'
#' @param dist_args List of arguments to `calc_dist()`
#' @param isolate Should the reactive expression be isolated? Will not return
#'   a reactive or take reactive dependencies if `TRUE`.
#'
#' @return A `reactive` containing the results of `calc_dist()`
#'
#' @keywords internal
reactive_dist <- function(dist_args, isolate = FALSE) {
  if (isolate) {
    shiny::isolate(calc_dist(
      vac = dist_args$vac(),
      inf = dist_args$inf(),
      symp = dist_args$symp(),
      test = dist_args$test(),
      detect = dist_args$detect()
    ))
  } else {
    reactive(calc_dist(
      vac = dist_args$vac(),
      inf = dist_args$inf(),
      symp = dist_args$symp(),
      test = dist_args$test(),
      detect = dist_args$detect()
    ), label = "reactive_dist()")
  }
}


#' Calculate the Steady-State Joint Distribution for a Population
#'
#' `calc_dist()` calculates the discrete joint distribution of vaccination,
#' infection, symptoms, tests, and detections in a population.
#'
#' @param vac `[list(3)]` A named list containing vaccination parameters:
#'   \describe{
#'     \item{p_comm `[numeric(1)]`}{Proportion vaccinated in the community}
#'     \item{p_org `[numeric(1)]`}{Proportion vaccinated in the organization of interest}
#'     \item{eff `[numeric(1)]`}{Vaccine efficacy}
#'   }
#'
#' @param inf `[list(3)]` A named list containing infection parameters:
#'   \describe{
#'     \item{p_incid `[numeric(1)]`}{Proportion of community newly infected each day}
#'     \item{t_symp `[numeric(1)]`}{Duration of symptomatic period}
#'     \item{t_presymp `[numeric(1)]`}{Duration of presymptomatic period}
#'   }
#'
#' @param symp `[list(3)]` A named lust containing symptom parameters:
#'   \describe{
#'     \item{p_inf_vac}{Proportion of vaccinated infections who are symptomatic}
#'     \item{p_inf_unvac}{Proportion of unvaccinated infections who are symptomatic}
#'     \item{p_uninf}{Proportion of uninfected people who are symptomatic}
#'   }
#'
#' @param test `[list(3)]` A named list containing testing parameters:
#'   \describe{
#'     \item{p_symp `[numeric(1)]`}{Probability of being tested if symptomatic}
#'     \item{p_asymp_vac `[numeric(1)]`}{Probability of being tested if asymptomatic and vaccinated}
#'     \item{p_asymp_unvac `[numeric(1)]`}{Probability of being tested if asymptomatic and unvaccinated}
#'   }
#'
#' @param detect `[list(2)]` A named list containing detection parameters:
#'   \describe{
#'     \item{sens `[numeric(1)]`}{Test sensitivity}
#'     \item{spec `[numeric(1)]`}{Test specificity}
#'   }
#'
#' @return A `data.table`
#'
#' @keywords internal
calc_dist <- function(
  # Vaccination parameters
  vac = list(p_comm = 0.5, p_org = 0.5, eff = 0.5),
  # Infection parameters
  inf = list(p_incid = 1e-3, t_symp = 5, t_presymp = 5),
  # Symptom parameters
  symp   = list(p_inf_vac = 0.5, p_inf_unvac = 0.5, p_uninf = 0),
  # Test parameters
  test   = list(p_symp = 1, p_asymp_vac = 1/7, p_asymp_unvac = 1/7),
  # Detection parameters
  detect = list(sens = 0.85, spec = 1)
) {

  # Create conditional distributions
  dt_vac    <- dist_vac(vac)
  dt_inf    <- dist_inf(inf, .vac = vac)
  dt_symp   <- dist_symp(symp, .inf = inf)
  dt_test   <- dist_test(test)
  dt_detect <- dist_detect(detect)

  # Create joint distribution
  dt_vac %>%
    join_dist(dt_inf) %>%
    join_dist(dt_symp) %>%
    join_dist(dt_test) %>%
    join_dist(dt_detect) %>%
    # Set key and reorder columns
    order_dist() %>%
    # Add params as attribute
    setattr(
      "params",
      list(vac = vac, inf = inf, symp = symp, test = test, detect = detect)
    )
}


# Join Distributions -----------------------------------------------------------

join_dist <- function(x, y) {
  # Manually add suffix to `p` in `y`
  setnames(y, "p", "p_y", skip_absent = TRUE)

  # Join by all common columns (now except probability `p`)
  nms_x <- names(x)
  by <- nms_x[nms_x %in% names(y)]

  # Left join
  d <- y[x, on = by, allow.cartesian = TRUE]

  # Fill missing conditional probs
  setnafill(d, fill = 1, cols = "p_y")

  # Multiply probabilities and remove conditional probs
  set(d, j = "p",   value = d$p * d$p_y)
  set(d, j = "p_y", value = NULL)
}


# Create Distributions ---------------------------------------------------------

dist_vac <- function(.vac) {
  data.table(
    # Probs conditional on vaccination status
    vac = c(TRUE, FALSE),
    p   = probs_vac(.vac)
  )
}

dist_inf <- function(.inf, .vac) {
  data.table(
    # Probs conditional on vaccination and infection status
    vac = c(TRUE, FALSE, TRUE, FALSE),
    inf = c(TRUE, TRUE, FALSE, FALSE),
    p   = probs_inf(.inf, vac = .vac)
  )
}

dist_symp <- function(.symp, .inf) {
  data.table(
    # Probs conditional on vaccination, infection, and symptomatic status
    vac  = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    inf  = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    symp = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    p    = probs_symp(.symp, .inf)
  )
}

dist_test <- function(.test) {
  data.table(
    # Probs conditional on vaccination, symptom, and test status
    vac  = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    symp = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    test = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    p    = probs_test(.test)
  )
}

dist_detect <- function(.detect) {
  data.table(
    # Probs conditional on infection, testing, and detection
    inf    = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    test   = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    detect = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    p      = probs_detect(.detect)
  )
}

# Probability Calculations -----------------------------------------------------

probs_vac <- function(vac) {
  # Add complement and return
  c(vac$p_org, 1 - vac$p_org)
}

probs_inf <- function(inf, vac) {
  # Uncorrected incidence "probabilities" within each group (may be > 1)
  p_incid_u <- inf$p_incid / (1 - vac$p_comm * vac$eff)
  p_incid_v <- p_incid_u * (1 - vac$eff)

  # Combine
  p_incid <- c(v = p_incid_v, u = p_incid_u)

  # Scale from incidence to infection probabilities (assuming constant t_inf)
  p_inf   <- pmin(1, p_incid * (inf$t_symp + inf$t_presymp))

  # Add complements and return
  # (order is {inf_vac, inf_unvac, uninf_vac, uninf_unvac})
  c(p_inf, 1 - p_inf)
}

probs_symp <- function(symp, inf) {
  # Account for presymptomatic illness
  p_t_symp <- (inf$t_symp / (inf$t_presymp + inf$t_symp))

  # Scale symptom probs by proportion of infection time spent symptomatic
  p_symp_inf <- c(symp$p_inf_vac, symp$p_inf_unvac) * p_t_symp

  # Combine all symptom probs
  # (order is {inf_vac, inf_unvac, uninf_vac, uninf_unvac})
  p_symp <- c(p_symp_inf, symp$p_uninf, symp$p_uninf)

  # Add complement and return
  c(p_symp, 1 - p_symp)
}

probs_test <- function(test) {
  # Combine asymptomatic probabilities
  p_asymp <- c(test$p_asymp_vac, test$p_asymp_unvac)

  # Symptomatic is <= asymptomatic (and length 2)
  p_symp <- test$p_symp + (1 - test$p_symp) * p_asymp

  # Combine symptomatic and asymptomatic
  # (order is {vac_symp, unvac_symp, vac_asymp, unvac_asymp})
  p_test <- c(p_symp, p_asymp)

  # Add complements and return
  c(p_test, 1 - p_test)
}

probs_detect <- function(detect) {
  # Combine and add probabilities for not tested (0)
  p_d <- c(detect$sens, 1 - detect$spec, 0, 0)

  # Add complements and return
  c(p_d, 1 - p_d)
}

order_dist <- function(dist) {
  cols <- c("p", "vac", "inf", "symp", "test", "detect")
  # Set column and row orders
  setcolorder(dist, cols)
  setorderv(dist, cols[2L:6L], order = -1L, na.last = TRUE)
}


assert_scalar_num <- function(x, lower = -Inf, upper = Inf, finite = FALSE) {
  checkmate::assert_numeric(
    x,
    lower = lower,
    upper = upper,
    finite = finite,
    any.missing = FALSE,
    len = 1L,
    null.ok = FALSE
  )
}


assert_scalar_prob <- function(x) {
  assert_scalar_num(x, lower = 0, upper = 1, finite = TRUE)
}


assert_param <- function(x, ref) {
  # Get name for error messages
  lbl <- rlang::expr_label(rlang::enexpr(x))
  force(lbl)

  # Check list
  checkmate::assert_list(
    x,
    types = "numeric",
    any.missing = FALSE,
    max.len = 3L,
    names = "unique",
    null.ok = FALSE,
    .var.name = lbl
  )
  # Check names
  checkmate::assert_subset(
    names(x),
    choices = names(ref),
    empty.ok = FALSE,
    .var.name = paste0("names(", lbl, ")")
  )
}
