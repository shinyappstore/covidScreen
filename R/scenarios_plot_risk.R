plot_risk <- function(data_risk) {
  data_risk %>%
    hchart("item",
      hcaes(name = .data$group, y = .data$n, color = .data$color),
      layout = "horizontal",
      itemPadding = 0
    ) %>%
    hc_legend(align = "center", verticalAlign = "bottom") %>%
    hc_tooltip(
      headerFormat = "<span style='color: {point.color}'>{point.key}</span><br>",
      pointFormat = "<b>{point.y}</b> {point.label}<br><b>{point.pct}%</b> of organization",
      valueDecimals = 0,
      borderWidth = 2
    ) %>%
    hc_boost(enabled = TRUE)
}

prep_risk <- function(data_test, data_no_test, n) {
  # Remove R CMD CHECK NOTE
  inf <- symp <- detect <- pct <- lbl <- i <- NULL

  # Prob of being detected b/c of symptoms only when infected
  p_d_symp <- data_no_test[inf & symp & detect, sum(.SD$p)][[1]]
  # Prob of being detected when infected
  p_d_all <- data_test[inf & detect, sum(.SD$p)][[1]]
  # Prob of being detected b/c of testing when infected
  p_d_test <- p_d_all - p_d_symp

  # Prob of not being detected when infected
  p_not_d <- data_test[inf & !detect, sum(.SD$p)][[1]]

  # Prob of not being infected
  p_not_i <- data_test[inf == FALSE, sum(.SD$p)][[1]]

  # Create data.table with groups
  data_risk <- data.table(
    group = c(
      "Undetected",
      "Detected w/ Testing",
      "Detected w/ Symptoms",
      "Not a Case"
    ),
    n     = c(  p_not_d,  p_d_test,  p_d_symp,   p_not_i) * n,
    color = c("#e57373", "#90caf9", "#b0bec5", "#e0e0e0")
  )

  data_risk[,
      pct := format_number(100 * .SD$n / sum(.SD$n))
  ][, n   := round(.SD$n)
  ][, lbl := fifelse(.SD$n == 1, yes = "person", no = "people")
  ][, i   := seq_len(.N)
  ][]
}
