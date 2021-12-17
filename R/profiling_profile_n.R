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
