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
calc_dist <- function(
  # Vaccination parameters
  vac = list(p_comm = 0.5, p_org = 0.5, eff = 0.7),
  # Infection parameters
  inf = list(p_incid = 1e-3, t_symp = 10, t_presymp = 3),
  # Symptom parameters
  symp   = list(p_inf_vac = 0.5, p_inf_unvac = 0.5, p_uninf = 0),
  # Test parameters
  test   = list(p_symp = 0.95, p_asymp_vac = 1/7, p_asymp_unvac = 1/7),
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
    order_dist()
}


#' Reactive Version of `calc_dist()`
#'
#' Takes a `reactiveValues` object and returns a `reactive`
#'
#' @param dist_args List of arguments to `calc_dist()`
#'
#' @return A `reactive` containing the results of `calc_dist()`
reactive_dist <- function(dist_args) {
  reactive(calc_dist(
    vac = dist_args$vac(),
    inf = dist_args$inf(),
    symp = dist_args$symp(),
    test = dist_args$test(),
    detect = dist_args$detect()
  ), label = "reactive_dist()")
}

# Join Distributions -----------------------------------------------------------

join_dist <- function(x, y) {
  if (is.null(x)) {
    return(y)
  } else if (is.null(y)) {
    return(x)
  }

  # Manually add suffix to `p` in `y`
  setnames(y, "p", "p_y", skip_absent = TRUE)

  # Join by all common columns (now except probability `p`)
  by <- intersect(colnames(x), colnames(y))

  # Left join
  d <- y[x, on = by, allow.cartesian = TRUE]

  # Fill missing conditional probs
  setnafill(d, fill = 1, cols = "p_y")

  # Multiply probabilities and remove conditional probs
  set(d, j = "p",   value = d$p * d$p_y)
  set(d, j = "p_y", value = NULL)
}

partial_dist <- function(
  vac = NULL,
  inf = NULL,
  symp = NULL,
  test = NULL,
  detect = NULL
) {
  # Create list of args
  arg_list <- purrr::map(
    rlang::fn_fmls(),
    ~ eval(.x, envir = rlang::env_parent())
  )

  # Find NULL args
  is_null <- purrr::map_lgl(arg_list, is.null)

  # Evaluate passed arguments
  dt_dist <- calc_dist(
    vac    = vac,
    inf    = inf,
    symp   = symp,
    test   = test,
    detect = detect
  )

  # Only calculate and join necessary distributions
  incl_dist <- "dt_dist"
  if (is.null(vac)) {
    incl_dist <- c(incl_dist, "join_dist(dist_vac(vac))")
  }
  if (is.null(inf)) {
    incl_dist <- c(incl_dist, "join_dist(dist_inf(inf, .vac = vac))")
  }
  if (is.null(symp)) {
    incl_dist <- c(incl_dist, "join_dist(dist_symp(symp, .inf = inf))")
  }
  if (is.null(test)) {
    incl_dist <- c(incl_dist, "join_dist(dist_test(test))")
  }
  if (is.null(detect)) {
    incl_dist <- c(incl_dist, "join_dist(dist_detect(detect))")
  }

  # Parse to expression
  dist_expr <- rlang::parse_expr(paste0(incl_dist, collapse = " %>% "))

  # Create new function with dist_expr as body
  partial_fn <- rlang::new_function(
    rlang::pairlist2(vac = , inf = , symp = , test = , detect = ),
    dist_expr
  )

  print(partial_fn)

  # Fill passed arguments and return new function
  purrr::partial(partial_fn, !!!arg_list[!is_null])
  arg_list[!is_null]
}

# Create Distributions ---------------------------------------------------------

dist_vac <- function(.vac) {
  if (is.null(.vac)) return(NULL)
  data.table(
    # Probs conditional on vaccination status
    vac = c(TRUE, FALSE),
    p   = probs_vac(.vac)
  )
}

dist_inf <- function(.inf, .vac) {
  if (is.null(.inf) || is.null(.vac)) return(NULL)
  data.table(
    # Probs conditional on vaccination and infection status
    vac = c(TRUE, FALSE, TRUE, FALSE),
    inf = c(TRUE, TRUE, FALSE, FALSE),
    p   = probs_inf(.inf, vac = .vac)
  )
}

dist_symp <- function(.symp, .inf) {
  if (is.null(.symp) || is.null(.inf)) return(NULL)
  data.table(
    # Probs conditional on vaccination, infection, and symptomatic status
    vac  = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    inf  = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    symp = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    p    = probs_symp(.symp, .inf)
  )
}

dist_test <- function(.test) {
  if (is.null(.test)) return (NULL)
  data.table(
    # Probs conditional on vaccination, symptom, and test status
    vac  = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    symp = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    test = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    p    = probs_test(.test)
  )
}

dist_detect <- function(.detect) {
  if (is.null(.detect)) return (NULL)
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
  p_presymp <- (1 - inf$t_presymp / (inf$t_presymp + inf$t_symp))

  # Combine symptoms probs for infections into scaled vector
  p_symp_inf <- c(symp$p_inf_vac, symp$p_inf_unvac) * p_presymp

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
  if (is.null(dist)) return (NULL)
  all_cols <- c("p", "vac", "inf", "symp", "test", "detect")
  cols     <- intersect(all_cols, colnames(dist))
  # Set column and row orders
  setcolorder(dist, cols)
  setorderv(dist, order = -1L, na.last = TRUE)
}
