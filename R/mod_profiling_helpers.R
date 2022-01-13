reactive_profile_y <- function(x, dist_args, n, y_lbl, i_nm, j_nm) {
  reactive({
    if (i_nm() == "n") {
      profile_n(
        x(),
        dist_args = dist_args,
        y_lbl = y_lbl()
      )()
    } else if (i_nm() == "test" && startsWith(j_nm(), "p_asymp")) {
      profile_test(
        x(),
        dist_args = dist_args,
        n     = n(),
        y_lbl = y_lbl(),
        j_nm  = j_nm()
      )()
    } else {
      profile_arg(
        x(),
        dist_args = dist_args,
        n     = n(),
        y_lbl = y_lbl(),
        i_nm  = i_nm(),
        j_nm  = j_nm()
      )()
    }
  }, label = "profile_y()")
}


# Profiling --------------------------------------------------------------------


profile_n <- function(n, dist_args, y_lbl, pct = FALSE) {
  # Calculate risk
  if (y_lbl == "risk") {
    d <- reactive_dist(dist_args)
    p_risk <- reactive(undetected(d()), label = "undetected()")

    reactive(p_risk() * (if (pct) 100 else n), label = "return_risk()")
    # Calculate benefit
  } else if (y_lbl == "benefit") {
    dist_args0 <- const_testing(dist_args)
    d0 <- reactive_dist(dist_args0)
    d  <- reactive_dist(dist_args)
    p_risk0   <- reactive(undetected(d0()), label = "undetected(d0)")
    p_risk    <- reactive(undetected(d()),  label = "undetected(d)")
    p_benefit <- reactive(p_risk0() - p_risk(), label = "p_benefit()")

    reactive(p_benefit() * (if (pct) 100 else n))
    # Calculate relative benefit in vac groups
  } else if (y_lbl == "vac") {
    dist_args0 <- const_testing(dist_args)
    dist_args1 <- const_testing(dist_args, 1, 1)
    d0 <- reactive_dist(dist_args0)
    d1 <- reactive_dist(dist_args1)
    slopes <- reactive(calc_vac_slopes(d0(), d1()), label = "calc_slopes()")

    reactive(rep(slopes()[[2]] / slopes()[[1]], times = NROW(n)))
  }
}


profile_test <- function(x, dist_args, n, y_lbl, j_nm, pct = FALSE) {
  if (y_lbl == "risk") {
    p_risk <- reactive_map_undetected(
      x,
      dist_args = dist_args,
      i_nm = "test",
      j_nm = j_nm
    )

    if (pct) reactive(p_risk() * 100) else reactive(p_risk() * n)
  } else if (y_lbl == "benefit") {
    dist_args0 <- const_testing(dist_args)
    d0     <- reactive_dist(dist_args0)
    p_risk0 <- reactive(undetected(d0()), label = "undetected()")
    p_risk <- reactive_map_undetected(
      x,
      dist_args = dist_args,
      i_nm = "test",
      j_nm = j_nm
    )
    p_benefit <- reactive(p_risk0() - p_risk(), label = "p_benefit()")

    if (pct) reactive(p_benefit() * 100) else reactive(p_benefit() * n)
  } else if (y_lbl == "vac") {
    dist_args0 <- const_testing(dist_args)
    dist_args1 <- const_testing(dist_args, 1, 1)
    d0 <- reactive_dist(dist_args0)
    d1 <- reactive_dist(dist_args1)
    slopes <- reactive(calc_vac_slopes(d0(), d1()), label = "calc_vac_slopes()")

    reactive(
      rep(slopes()[[2]] / slopes()[[1]], times = NROW(x)),
      label = "ratio()"
    )
  }
}


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


# Helpers ----------------------------------------------------------------------


reactive_map_vac <- function(new_arg_seq, dist_args, i_nm, j_nm) {
  reactive(purrr::flatten_dbl(purrr::map(
    new_arg_seq,
    ~ reactive_vac_mapper(
      .x,
      dist_args = dist_args,
      i_nm = i_nm,
      j_nm = j_nm
    )()
  )))
}


reactive_vac_mapper <- function(new_arg, dist_args, i_nm, j_nm) {
  args <- insert_args(new_arg, dist_args = dist_args, i_nm = i_nm, j_nm = j_nm)
  d0 <- reactive_dist(const_testing(args))
  d1 <- reactive_dist(const_testing(args, 1, 1))

  reactive(calc_vac_slopes(data_test0 = d0(), data_test1 = d1()))
}


reactive_map_ratio <- function(x_args0, x_args1) {
  reactive(
    purrr::map2(
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


reactive_map_undetected <- function(new_arg_seq, dist_args, i_nm, j_nm) {
  reactive(purrr::map_dbl(
      new_arg_seq,
      ~ undetected(reactive_map_dist(.x, dist_args, i_nm, j_nm)())
    ),
    label = "map_undetected()"
  )
}


reactive_map_detect <- function(new_arg_seq, dist_args, i_nm, j_nm) {
  reactive(rbindlist(purrr::map(
    new_arg_seq,
    ~ reactive_detect_mapper(
      .x,
      dist_args = dist_args,
      i_nm = i_nm,
      j_nm = j_nm
    )()
  )), label = "map_detect()")
}


reactive_detect_mapper <- function(new_arg, dist_args, i_nm, j_nm) {
  d <- reactive_dist(insert_args(new_arg, dist_args, i_nm, j_nm))

  reactive(data.table(
      p_risk = undetected(d()),
      p_d_all  = detected(d()),
      p_d_symp = detected(d(), symp = TRUE),
      p_d_test = detected(d(), symp = FALSE)
  ), label = "detect_mapper()")
}

#' @export
undetected <- function(dt) {
  sum(dt$p[dt$inf & !dt$detect])
}


detected <- function(dt, symp = NULL) {
  checkmate::assert_logical(symp, max.len = 1, null.ok = TRUE)
  if (is.null(symp) || is.na(symp)) {
    sum(dt$p[dt$inf & dt$detect])
  } else if (symp) {
    sum(dt$p[dt$inf & dt$detect & dt$symp])
  } else {
    sum(dt$p[dt$inf & dt$detect & !dt$symp])
  }
}


#' @export
risk_reduction <- function(dt, relative = TRUE) {
  checkmate::assert_logical(relative, any.missing = FALSE, len = 1)
  p0 <- const_testing(attr(dt, "params", exact = TRUE), p_vac = 0, p_unvac = 0)
  u0 <- undetected(do.call(calc_dist, p0))
  reduction <- u0 - undetected(dt)
  if (relative) reduction / u0 else reduction
}

false_positive <- function(dt) {
  sum(dt$p[!dt$inf & dt$detect])
}

false_negative <- function(dt) {
  sum(dt$p[dt$inf & !dt$detect])
}

seq_profile <- function(x, n = 55, intervals = c(1, 2, 5, 10)) {
  if (NROW(x) == 1) return(x)
  # Ensure that x is length 2 numeric
  checkmate::assert_numeric(x, len = 2L, finite = TRUE, any.missing = FALSE)

  # Range
  r <- abs(diff(x))

  # Magnitude of intervals
  m <- floor(log10(r)) - floor(log10(n))

  # Scale candidate intervals
  intervals <- intervals * 10^m
  # Expand to +/- 1 order of magnitude
  intervals <- unique(c(intervals * 1e-1, intervals, intervals * 1e1))

  # Must yield `n` intervals or fewer; pick smallest from remaining
  by <- min(intervals[r / intervals <= n])

  # Create sequence; count down if x[[1]] is larger than x[[2]]
  s <- seq(x[[1]], x[[2]], by = if (x[[1]] > x[[2]]) -by else by)

  # Ensure endpoints are always included
  if (x[[2]] != s[[NROW(s)]]) c(s, x[[2]]) else s
}
