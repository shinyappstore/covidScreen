plot_vac <- function(data_vac) {
  data_vac %>%
    hchart("column", hcaes(x = x, y = benefit, color = color)) %>%
    hc_xAxis(categories = data_vac$group)
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
  # Vaccinated - no testing
  p_t_v_t0 <- data_test0[vac == TRUE & test == TRUE, sum(.SD$p)][[1]]
  p_d_v_t0 <- data_test0[vac == TRUE & inf == TRUE & detect == TRUE,
                         sum(.SD$p)][[1]]

  # Vaccinated - testing
  p_t_v_t1 <- data_test1[vac == TRUE & test == TRUE, sum(.SD$p)][[1]]
  p_d_v_t1 <- data_test1[vac == TRUE & inf == TRUE & detect == TRUE,
                         sum(.SD$p)][[1]]

  # Unvaccinated - no testing
  p_t_u_t0 <- data_test0[vac == FALSE & test == TRUE, sum(.SD$p)][[1]]
  p_d_u_t0 <- data_test0[vac == FALSE & inf == TRUE & detect == TRUE,
                         sum(.SD$p)][[1]]

  # Unvaccinated - testing
  p_t_u_t1 <- data_test1[vac == FALSE & test == TRUE, sum(.SD$p)][[1]]
  p_d_u_t1 <- data_test1[vac == FALSE & inf == TRUE & detect == TRUE,
                         sum(.SD$p)][[1]]

  c(
    v = (p_d_v_t1 - p_d_v_t0) / (p_t_v_t1 - p_t_v_t0),
    u = (p_d_u_t1 - p_d_u_t0) / (p_t_u_t1 - p_t_u_t0)
  )
}
