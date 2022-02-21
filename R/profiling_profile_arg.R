profile_arg <- function(x, dist_args, n, y_lbl, i_nm, j_nm, pct = FALSE) {
  if (y_lbl == "risk") {
    p_risk <- reactive_map_undetected(
      x,
      dist_args = dist_args,
      i_nm = i_nm,
      j_nm = j_nm
    )

    if (pct) reactive(p_risk() * 100) else reactive(p_risk() * n)
  } else if (y_lbl == "benefit") {
    dist_args0 <- const_testing(dist_args)
    p_risk0 <- reactive_map_undetected(
      x,
      dist_args = dist_args0,
      i_nm = i_nm,
      j_nm = j_nm
    )
    p_risk  <- reactive_map_undetected(
      x,
      dist_args = dist_args,
      i_nm = i_nm,
      j_nm = j_nm
    )
    p_benefit <- reactive(p_risk0() - p_risk())

    if (pct) reactive(p_benefit() * 100) else reactive(p_benefit() * n)
  } else if (y_lbl == "vac") {
    dist_args0 <- const_testing(dist_args)
    dist_args1 <- const_testing(dist_args, 1, 1)
    x_args0 <- purrr::map(x, ~ insert_args(.x, dist_args0, i_nm, j_nm))
    x_args1 <- purrr::map(x, ~ insert_args(.x, dist_args1, i_nm, j_nm))

    reactive_map_ratio(x_args0, x_args1)
  }
}


reactive_map_ratio <- function(x_args0, x_args1) {
  reactive(
    purrr::map2_dbl(
      x_args0,
      x_args1,
      ~ calc_ratio(calc_vac_slopes(reactive_dist(.x)(), reactive_dist(.y)()))
    ),
    label = "map_ratio()"
  )
}


calc_ratio <- function(slopes) {
  slopes[[2]] / slopes[[1]]
}
