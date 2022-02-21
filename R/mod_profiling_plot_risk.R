profiling_plot_risk <- function(data, i_nm, j_nm) {
  x_title <- profile_arg_labels[[i_nm]][[j_nm]]
  tt_decimals <- -floor(log10(max(data$n, na.rm = TRUE))) + 2
  data %>%
    hchart(
      "area",
      hcaes(
        name  = forcats::as_factor(.data$group),
        group = forcats::as_factor(.data$group),
        color = .data$color,
        x = .data$x,
        y = .data$n
      ),
      stacking = "normal",
      marker = list(enabled = FALSE, symbol = "circle")
    ) %>%
    hc_colors(c("#b0bec5", "#90caf9", "#e57373")) %>%
    hc_xAxis(title = list(text = x_title)) %>%
    hc_yAxis(title = list(text = "Cases")) %>%
    hc_tooltip(
      shared = TRUE,
      valueDecimals = tt_decimals,
      headerFormat = paste0("<b>", x_title, ": {point.x}</b><br>"),
      pointFormat = paste0(
        "<span style='color: {point.color}; font-weight: bold'>",
        "{point.name}: {point.y}</span> people<br>"
      )
    )
}

profiling_prep_risk <- function(x, x_t, n, dist_args, i_nm, j_nm) {
  if (i_nm == "n") {
    d0 <- reactive_dist(const_testing(dist_args))
    d  <- reactive_dist(dist_args)

    # Prob of being detected b/c of symptoms only when infected
    p_d_symp <- reactive(detected(d0(), symp = TRUE))
    # Prob of being detected b/c of testing when infected
    p_d_test <- reactive(detected(d()) - p_d_symp())
    # Prob of not being detected when infected
    p_not_d <- reactive(undetected(d()))

    reactive(data.table(
      group = rep(c(
        "Detected w/ Symptoms",
        "Detected w/ Testing",
        "Undetected"
      ), NROW(x_t)),
      x = rep(x, each = 3),
      n = x_t %>%
        purrr::map(~ .x * c(p_d_symp(), p_d_test(), p_not_d())) %>%
        purrr::flatten_dbl(),
      color = rep(c("#b0bec5", "#90caf9", "#e57373"), NROW(x))
    ))
  } else if (i_nm == "test" && startsWith(j_nm, "p_asymp")) {
    d0 <- reactive_dist(const_testing(dist_args))
    d  <- reactive_map_detect(
      x_t,
      dist_args = dist_args,
      i_nm = "test",
      j_nm = j_nm
    )

    # Prob of being detected b/c of symptoms only when infected
    p_d_symp <- reactive(rep(detected(d0(), symp = TRUE), times = NROW(x_t)))
    # Prob of being detected b/c of testing when infected
    p_d_test <- reactive(d()$p_d_all - p_d_symp())
    # Prob of not being detected when infected
    p_not_d <- reactive(d()$p_risk)

    reactive(data.table(
      group = rep(c(
        "Detected w/ Symptoms",
        "Detected w/ Testing",
        "Undetected"
      ), NROW(x_t)),
      x = rep(x, each = 3),
      n = list(p_d_symp(), p_d_test(), p_not_d()) %>%
        purrr::pmap(~ n * c(..1, ..2, ..3)) %>%
        purrr::flatten_dbl(),
      color = rep(c("#b0bec5", "#90caf9", "#e57373"), NROW(x))
    ))
  } else {
    dist_args0 <- const_testing(dist_args)
    d0 <- reactive_map_detect(
      x_t,
      dist_args = dist_args0,
      i_nm = i_nm,
      j_nm = j_nm
    )
    d <- reactive_map_detect(
      x_t,
      dist_args = dist_args,
      i_nm = i_nm,
      j_nm = j_nm
    )

    # Prob of being detected b/c of symptoms only when infected
    p_d_symp <- reactive(d0()$p_d_symp)
    # Prob of being detected b/c of testing when infected
    p_d_test <- reactive(d()$p_d_all - p_d_symp())
    # Prob of not being detected when infected
    p_not_d <- reactive(d()$p_risk)

    reactive(data.table(
      group = rep(c(
        "Detected w/ Symptoms",
        "Detected w/ Testing",
        "Undetected"
      ), NROW(x_t)),
      x = rep(x, each = 3),
      n = list(p_d_symp(), p_d_test(), p_not_d()) %>%
        purrr::pmap(~ n * c(..1, ..2, ..3)) %>%
        purrr::flatten_dbl(),
      color = rep(c("#b0bec5", "#90caf9", "#e57373"), NROW(x))
    ))
  }
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


undetected <- function(dt) {
  sum(dt$p[dt$inf & !dt$detect])
}
