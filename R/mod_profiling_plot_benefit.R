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
        y = .data$n,
        color = .data$color
      ),
      stacking = "normal",
      marker = list(enabled = FALSE, symbol = "circle")
    ) %>%
    hc_colors(c("#90caf9", "#e57373")) %>%
    hc_xAxis(title = list(text = x_title)) %>%
    hc_yAxis(title = list(text = "% of Asymptomatic Cases"), max = 101, endOnTick = FALSE) %>%
    hc_tooltip(
      shared = TRUE,
      valueDecimals = tt_decimals,
      headerFormat = paste0("<b>", x_title, ": {point.x}</b><br>"),
      pointFormat = paste0(
        "<span style='color: {point.color}; font-weight: bold'>",
        "{point.name}: {point.y}%</span> of asymptomatic cases<br>"
      )
    )
}


profiling_prep_benefit <- function(data_risk) {
  # Remove R CMD CHECK notes
  group <- NULL
  d <- data_risk[
    group != "Detected w/ Symptoms",
    list(
      group = .SD$group,
      n = 100 * .SD$n / sum(.SD$n),
      color = .SD$color
    ),
    by = "x"
  ]

  set(d, i = which(d$group == "Detected w/ Testing"), j = "group", value = "Detected")
}
