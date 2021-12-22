#' profiling_output2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_profiling_output_ui <- function(id){
  ns <- NS(id)
  highchartOutput(ns("plot"))
}

#' profiling_output2 Server Functions
#'
#' @noRd
mod_profiling_output_server <- function(id, inputs){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Get name of list that contains profile variable
    i_nm <- reactive(
      stringr::str_extract(inputs$vars$x(), "^[^_]+"),
      label = "i_nm()"
    )
    # Get name of profile variable in list
    j_nm <- reactive(
      stringr::str_remove(inputs$vars$x(), "^[^_]+_"),
      label = "j_nm()"
    )

    # Get profile variable
    x_arg <- reactive_x_arg(inputs, i_nm = i_nm, j_nm = j_nm)
    # Create x axis points
    x     <- reactive(seq_profile(x_arg()), label = "seq_profile()")

    # Transform dist_args to appropriate model input
    dist_args_t <- trans_dist_args(inputs$dist_args)
    # Transform x to appropriate model input
    x_t         <- reactive(trans_x(x(), j_nm()), label = "trans_x()")
    # Transform j_nm to match model input
    j_nm_t      <- reactive(trans_j_nm(j_nm()), label = "trans_j_nm()")

    # Create y
    y <- reactive_profile_y(
      x = x_t,
      dist_args = dist_args_t,
      n = inputs$n$org,
      y_lbl = inputs$vars$y,
      i_nm = i_nm,
      j_nm = j_nm_t
    )

    output$plot <- renderHighchart(
      hchart(data.table(x = x(), y = y()), "line", hcaes(x = x, y = y))
    )
  })
}


reactive_x_arg <- function(inputs, i_nm, j_nm) {
  reactive({
    if (i_nm() == "n") {
      inputs$n$org()
    } else {
      inputs$dist_args[[i_nm()]]()[[j_nm()]]
    }
  }, label = "x_arg()")
}


trans_dist_args <- function(dist_args) {
  trans_args <- reactiveValues()

  trans_args$vac <- reactive(purrr::map(
    dist_args$vac(),
    ~ .x * 1e-2
  ), label = "trans_vac()")

  trans_args$inf <- reactive({
    new_nms <- stringr::str_replace(names(dist_args$inf()), "^r_", "p_")
    dist_args$inf() %>%
      purrr::map_if(startsWith(names(.), "r_"), ~ .x * 1e-5) %>%
      magrittr::set_names(new_nms)
  }, label = "trans_inf()")

  trans_args$symp <- reactive(purrr::map(
    dist_args$symp(),
    ~ .x * 1e-2
  ), label = "trans_symp()")

  trans_args$test <- reactive({
    new_nms <- stringr::str_replace(names(dist_args$test()), "^f_", "p_")
    dist_args$test() %>%
      purrr::imap(
        ~ (if (startsWith(.y, "f_")) correct_freq(1 / .x) else .x * 1e-2)
      ) %>%
      magrittr::set_names(new_nms)
  }, label = "trans_test()")

  trans_args$detect <- reactive(purrr::map(
    dist_args$detect(),
    ~ .x * 1e-2
  ), label = "trans_detect()")

  trans_args
}

trans_x <- function(x, j_nm) {
  if (startsWith(j_nm, "f_")) {
    correct_freq(1 / x)
  } else if (startsWith(j_nm, "r_")) {
    x * 1e-5
  } else if (startsWith(j_nm, "p_")) {
    x * 1e-2
  } else if (j_nm %in% c("eff", "sens", "spec")) {
    x * 1e-2
  } else {
    x
  }
}


trans_j_nm <- function(j_nm) {
  if (any(startsWith(j_nm, c("f_", "r_")))) {
    stringr::str_replace(j_nm, "^[a-z]_", "p_")
  } else {
    j_nm
  }
}


## To be copied in the UI
# mod_profiling_output2_ui("profiling_output2_ui_1")

## To be copied in the server
# mod_profiling_output2_server("profiling_output2_ui_1")
