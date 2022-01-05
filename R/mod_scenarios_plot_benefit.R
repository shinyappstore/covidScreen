plot_benefit <- function(data_benefit) {
  data_benefit %>%
    hchart("column",
      hcaes(y = .data$n, name = .data$group, group = .data$group, color = .data$color),
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
    ) %>%
    hc_tooltip(
      headerFormat = "<span style='color: {point.color}'>{point.key}</span><br>",
      pointFormat = "<b>N = {point.y:.0f}</b>"
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

  d <- data.table(
    group = c("Undetected", "Detected"),
    n     = c(p_not_d, p_d_test) * n,
    color = c("#ef9a9a", "#64b5f6")
  )

  # Create columns
  set(d, j = "pct",     value = 100 * d$n / sum(d$n))
  set(d, j = "pct_lbl", value = paste0(round(d$pct), "%"))
  set(d, j = "lbl",     value = fifelse(d$pct < 1,
                                        NA_character_,
                                        paste(d$pct_lbl, d$group))
  )
}
