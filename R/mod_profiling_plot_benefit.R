profiling_plot_benefit <- function(data, i_nm, j_nm) {
  x_title <- profile_arg_labels[[i_nm]][[j_nm]]
  tt_decimals <- -floor(log10(max(data$n, na.rm = TRUE))) + 2
  data %>%
    hchart(
      "area",
      hcaes(
        name  = forcats::as_factor(.data$group),
        group = forcats::as_factor(.data$group),
        x = .data$x,
        y = .data$n
      ),
      stacking = "normal",
      marker = list(enabled = FALSE, symbol = "circle")
    ) %>%
    hc_colors(c("#90caf9", "#e57373")) %>%
    hc_xAxis(title = list(text = x_title)) %>%
    hc_yAxis(title = list(text = "% of Asymptomatic Cases")) %>%
    hc_tooltip(shared = TRUE, valueDecimals = tt_decimals)
}


profiling_prep_benefit <- function(data_risk) {
  data_risk[
    group != "Detected w/ Symptoms",
    list(
      group = .SD$group,
      n = .SD$n / sum(.SD$n)
    ),
    by = "x"
  ]
}
