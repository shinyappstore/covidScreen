plot_benefit <- function(data_benefit) {
  data_benefit %>%
    hchart("column",
      hcaes(y = .data$n, group = .data$group, color = .data$color),
      showInLegend = FALSE,
      stacking = "normal",
      dataLabels = list(
        enabled = TRUE,
        format = "{point.lbl}",
        style = list(textOutline = "none")
      )
    ) %>%
    hc_xAxis(
      title = list(text = "Asymptomatic Cases"),
      labels = list(enabled = FALSE),
      tickColor = NA
    ) %>%
    hc_yAxis(
      title = list(text = "N")
    )
}

prep_benefit <- function(data_test, data_no_test, n) {
  # Remove R CMD CHECK NOTE
  inf <- symp <- detect <- pct <- pct_lbl <- lbl <- NULL

  # Prob of being detected b/c of symptoms only when infected
  p_d_symp <- data_no_test[inf & symp & detect, sum(.SD$p)][[1]]
  # Prob of being detected when infected
  p_d_all <- data_test[inf & detect, sum(.SD$p)][[1]]
  # Prob of being detected b/c of testing when infected
  p_d_test <- p_d_all - p_d_symp

  # Prob of not being detected when infected
  p_not_d <- data_test[inf & !detect, sum(.SD$p)][[1]]
  p_asymp <- p_d_test + p_not_d

  data_benefit <- data.table(
    group = c("undetected", "detected"),
    n     = c(p_not_d, p_d_test) * n,
    color = c("#ef9a9a", "#64b5f6")
  )

  data_benefit[,
      pct := 100 * .SD$n / sum(.SD$n)
  ][, pct_lbl := paste0(round(.SD$pct), "% ")
  ][, lbl := fifelse(.SD$pct < 1, NA_character_, paste0(.SD$pct_lbl, .SD$group))
  ]
}
