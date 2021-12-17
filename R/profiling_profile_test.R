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
    p_risk0 <- reactive(undetected(.x()), label = "undetected()")
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


insert_args <- function(new_arg, dist_args, i_nm, j_nm = NULL) {
  new_args <- reactiveValues()
  if (i_nm != "vac") {
    new_args$vac <- reactive(dist_args$vac())
  }
  if (i_nm != "inf") {
    new_args$inf <- reactive(dist_args$inf())
  }
  if (i_nm != "symp") {
    new_args$symp <- reactive(dist_args$symp())
  }
  if (i_nm != "test") {
    new_args$test <- reactive(dist_args$test())
  }
  if (i_nm != "detect") {
    new_args$detect <- reactive(dist_args$detect())
  }

  if (is.null(j_nm)) {
    new_args[[i_nm]] <- new_arg
  } else {
    new_i <- reactive(
        magrittr::inset2(dist_args[[i_nm]](), j_nm, new_arg),
        label = "insert_j_arg()"
    )

    new_args[[i_nm]] <- new_i
  }

  new_args
}


map_undetected <- function(new_arg_seq, dist_args, i_nm, j_nm) {
  reactive(
    purrr::map_dbl(
      new_arg_seq,
      ~ undetected(reactive_dist(insert_args(.x, dist_args, i_nm, j_nm))())
    ),
    label = "map_arg_undetected()"
  )
}
