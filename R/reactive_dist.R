#' Update the Joint Distribution in Shiny
#'
#' @param data A `data.table` containing the current distribution, as created
#'   by `calc_dist()`
#' @inheritParams calc_dist
#'
#' @return A `reactive` containing the updated distribution
reactive_dist <- function(
  data = calc_dist(),
  vac,
  inf,
  symp,
  test,
  detect
) {
  # Create conditional distributions
  dt_vac    <- reactive(dist_vac(vac))
  dt_inf    <- reactive(dist_inf(inf, .vac = vac))
  dt_symp   <- reactive(dist_symp(symp, .inf = inf))
  dt_test   <- reactive(dist_test(test))
  dt_detect <- reactive(dist_detect(detect))

  dt <- reactive(data)
  dt <- reactive(update_p(dt(), "vac", dt_vac()))
  dt <- reactive(update_p(dt(), "inf", dt_inf()))
  dt <- reactive(update_p(dt(), "symp", dt_symp()))
  dt <- reactive(update_p(dt(), "test", dt_test()))
  dt <- reactive(update_p(dt(), "detect", dt_detect()))

  dt
}

#' Update the Probabilities for the Joint Distribution
#'
#' @param data A `data.table` containing the current distribution, as created
#'   by `calc_dist()`
#' @param var A string indicating the variable probability to update
#' @param dt_p A `data.table` of probabilities and variable values for joining
#'
#' @return The data with updated probabilities
update_p <- function(data, var, dt_p) {
  # Get original probabilities for variable
  dt_var <- attr(data, var)
  # Create multiplier for new probabilities
  dt_var["p" := dt_p$p / .SD$p]

  # Join to data to update probabilities
  dt <- join_dist(data, dt_var)

  # Update var attribute
  attr(data, var) <- dt_p

  dt
}
