#' Update the Joint Distribution in Shiny
#'
#' @param dist_args `[reactiveValues]` A `reactiveValues` object containing
#'   reactives corresponding to the inputs to `calc_dist()`
#'
#' @return A `reactive` containing the updated distribution
reactive_dist <- function(dist_args) {
  # Update conditional distributions
  dt <- reactive_conditional(dist_args)

  dt_sd <- reactive(join_dist(dt$symp(), dt$detect()), label = "dt_sd()")
  dt_isd <- reactive(join_dist(dt$inf(), dt_sd()), label = "dt_isd()")
  dt_visd <- reactive(join_dist(dt$vac(), dt_isd()), label = "dt_visd()")

  reactive({
    join_dist(dt_visd(), dt$test()) %>%
      setcolorder(c("p", "vac", "inf", "symp", "test", "detect")) %>%
      setorderv(order = -1L, na.last = TRUE)
  }, label = "dt_vistd()")
}

reactive_conditional <- function(dist_args) {
  dt <- reactiveValues()

  dt$vac <- reactive(dist_vac(dist_args$vac()),
                     label = "dt$vac()")
  dt$inf <- reactive(dist_inf(dist_args$inf(), .vac = dist_args$vac()),
                     label = "dt$inf()")
  dt$symp <- reactive(dist_symp(dist_args$symp(), .inf = dist_args$inf()),
                      label = "dt$symp()")
  dt$test <- reactive(dist_test(dist_args$test()),
                      label = "dt$test()")
  dt$detect <- reactive(dist_detect(dist_args$detect()),
                        label = "dt$detect()")
  dt
}

reactive_dist_v <- function(dt) {
  dt_vi <- reactive(join_dist(dt$vac(), dt$inf()), label = "dt_vi()")
  dt_std <- reactive(join_dist(join_dist(dt$symp(), dt$test()), dt$detect()),
                     label = "dt_std()")

  reactive(order_dist(join_dist(dt_vi(), dt_std())), label = "dist_v()")
}

reactive_dist_is <- function(dt) {
  dt_is <- reactive(join_dist(dt$inf(), dt$symp()), label = "dt_is()")
  dt_vtd <- reactive(join_dist(join_dist(dt$vac(), dt$test()), dt$detect()),
                     label = "dt_vtd()")

  reactive(order_dist(join_dist(dt_is(), dt_vtd())), label = "dist_i()")
}

reactive_dist_s <- function(dt) {
  dt_vitd <- reactive(
    join_dist(join_dist(join_dist(dt$vac(), dt$inf()), dt$test()), dt$detect()),
    label = "dt_vitd()")

  reactive(order_dist(join_dist(dt_vitd(), dt$symp())), label = "dist_s()")
}

reactive_dist_t <- function(dt) {
  dt_visd <- reactive(
    join_dist(join_dist(join_dist(dt$vac(), dt$inf()), dt$symp()), dt$detect()),
    label = "dt_visd()")

  reactive(order_dist(join_dist(dt_visd(), dt$test())), label = "dist_t()")
}

reactive_dist_d <- function(dt) {
  dt_vist <- reactive(
    join_dist(join_dist(join_dist(dt$vac(), dt$inf()), dt$symp()), dt$test()),
    label = "dt_vist()")

  reactive(order_dist(join_dist(dt_vist(), dt$detect())), label = "dist_d()")
}

reactive_dist_vis <- function(dt) {
  dt_vis <- reactive(join_dist(join_dist(dt$vac(), dt$inf()), dt$symp()),
                     label = "dt_vis()")
  dt_td <- reactive(join_dist(dt$test(), dt$detect()), label = "dt_itd()")

  reactive(order_dist(join_dist(dt_vis(), dt_td())), label = "dist_vis()")
}

reactive_dist_vt <- function(dt) {
  dt_vit <- reactive(join_dist(join_dist(dt$vac(), dt$inf()), dt$test()),
                     label = "dt_vit()")
  dt_sd <- reactive(join_dist(dt$symp(), dt$detect()), label = "dt_sd()")

  reactive(order_dist(join_dist(dt_vit(), dt_sd())), label = "dist_vt()")
}

reactive_dist_vd <- function(dt) {
  dt_vid <- reactive(join_dist(join_dist(dt$vac(), dt$inf()), dt$detect()),
                     label = "dt_vit()")
  dt_st <- reactive(join_dist(dt$symp(), dt$test()), label = "dt_st()")

  reactive(order_dist(join_dist(dt_vid(), dt_st())), label = "dist_vd()")
}
