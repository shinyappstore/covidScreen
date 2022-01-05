plot_vac <- function(data_vac) {
  data_vac %>%
    hchart("column",
      hcaes(name = .data$group, x = .data$x, y = .data$benefit, color = .data$color),
      showInLegend = FALSE,
      dataLabels = list(
        enabled = TRUE,
        format = "<span style='color: {point.color}'>{point.y:.1f}%</span>",
        style = list(textOutline = "none")
      )
    ) %>%
    hc_xAxis(categories = data_vac$group, title = list(enabled = FALSE)) %>%
    hc_yAxis(title = list(text = "Cases Detected per 100 Tests")) %>%
    hc_tooltip(
      headerFormat = "<span style='color: {point.color}'>{point.key}</span><br>",
      pointFormat = "<b>% Positive Tests: {point.y:.1f}%</b>"
    )
}

prep_vac <- function(data_test0, data_test1) {
  s <- calc_vac_slopes(data_test0 = data_test0, data_test1 = data_test1)

  data.table(
    group   = c("Vaccinated", "Unvaccinated"),
    benefit = s * 100,
    color   = c("#90caf9", "#90a4ae"),
    x       = seq_along(s) - 1
  )
}

calc_vac_slopes <- function(data_test0, data_test1) {
  # Deal with R CMD CHECK NOTE
  vac <- inf <- test <- detect <- NULL

  # No testing
  p_t_t0 <- setorderv(
    data_test0[test == TRUE, sum(.SD$p), by = "vac"],
    cols = "vac",
    order = -1L
  )[[2]]
  p_d_t0 <- setorderv(
    data_test0[inf & detect, sum(.SD$p), by = "vac"],
    cols = "vac",
    order = -1L
  )[[2]]

  # Testing
  p_t_t1 <- setorderv(
    data_test1[test == TRUE, sum(.SD$p), by = "vac"],
    cols = "vac",
    order = -1L
  )[[2]]
  p_d_t1 <- setorderv(
    data_test1[inf & detect, sum(.SD$p), by = "vac"],
    cols = "vac",
    order = -1L
  )[[2]]

  # Calculate slopes and return
  slopes <- (p_d_t1 - p_d_t0) / (p_t_t1 - p_t_t0)
  names(slopes) <- c("v", "u")
  slopes
}
