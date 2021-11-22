#' Update the Joint Distribution in Shiny
#'
#' @param data A `data.table` containing the current distribution, as created
#'   by `calc_dist()`
#' @inheritParams calc_dist
#'
#' @return An `observer` containing the updated distribution
update_dist <- function(
  data,
  vac,
  inf,
  symp,
  test,
  detect
) {
  # Create conditional distributions
  dt_vac    <- observe(dist_vac(vac))
  dt_inf    <- observe(dist_inf(inf, .vac = vac))
  dt_symp   <- observe(dist_symp(symp, .inf = inf))
  dt_test   <- observe(dist_test(test))
  dt_detect <- observe(dist_detect(detect))

  dt <- observe(data)
  dt <- observe(join_dist_update(dt(), dt_vac()))
  dt <- observe(join_dist_update(dt(), dt_inf()))
  dt <- observe(join_dist_update(dt(), dt_symp()))
  dt <- observe(join_dist_update(dt(), dt_test()))
  dt <- observe(join_dist_update(dt(), dt_detect()))

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
  # Create multiplier for new probabilities (shallow copy)
  dt_var$p <- dt_p$p / dt_var$p

  # Join to data to update probabilities
  dt <- join_dist(data, dt_var)

  # Update var attribute
  attr(data, var) <- dt_p

  dt
}
