reactive_profile_y <- function(x, dist_args, n, y_lbl, i_nm, j_nm) {
  reactive({
    print(dist_args$vac())
    if (i_nm() == "n") {
      profile_n(
        x(),
        dist_args = dist_args,
        y_lbl = y_lbl()
      )
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


profile_n <- function(n, dist_args, y_lbl, pct = FALSE) {
  # Calculate risk
  if (y_lbl == "risk") {
    p_risk <- dist_args %>%
      reactive_dist(isolate = TRUE) %>%
      undetected()

    p_risk * (if (pct) 100 else n)
    # Calculate benefit
  } else if (y_lbl == "benefit") {
    dist_args0 <- const_testing(dist_args)
    p_risk0 <- dist_args0 %>%
      reactive_dist(isolate = TRUE) %>%
      undetected()
    p_risk  <- dist_args %>%
      reactive_dist(isolate = TRUE) %>%
      undetected()
    p_benefit <- p_risk0 - p_risk

    p_benefit * (if (pct) 100 else n)
    # Calculate relative benefit in vac groups
  } else if (y_lbl == "vac") {
    dist_args0 <- const_testing(dist_args)
    dist_args1 <- const_testing(dist_args, 1, 1)
    d0 <- reactive_dist(dist_args0, isolate = TRUE)
    d1 <- reactive_dist(dist_args1, isolate = TRUE)
    slopes <- calc_vac_slopes(d0, d1)

    rep(slopes[[2]] / slopes[[1]], times = NROW(n))
  }
}


undetected <- function(dt) {
  dt[inf & !detect, sum(.SD$p)][[1]]
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


freq_to_prob <- function(x) {
  p <- purrr::map_if(x, startsWith(names(x), "f_"), ~ correct_freq(1 / .x))
  names(p) <- stringr::str_replace(names(x), "^f_", "p_")
  p
}
