#' profiling_output UI Function
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

#' profiling_output Server Functions
#'
#' @noRd
mod_profiling_output_server <- function(id, inputs){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Extract reactiveValues objects from inputs
    n <- inputs$n
    dist_args <- inputs$dist_args
    vars <- inputs$vars

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
    x_arg <- reactive(
      if (i_nm() == "n") inputs$n$org() else inputs$dist_args[[i_nm()]]()[[j_nm()]],
      label = "x_arg()"
    )

    # Create x axis points
    x <- reactive(seq_profile(x_arg()), label = "seq_profile()")

    # Convert test frequencies to probabilities
    test_p <- reactive(
      freq_to_prob(inputs$dist_args$test()),
      label = "freq_to_prob()"
    )

    # Insert into dist_args
    dist_args_p <- insert_args(test_p, inputs$dist_args, "test")

    # Update j_nm if needed
    j_nm2 <- reactive(stringr::str_replace(j_nm(), "f_", "p_"))

    # Update x if needed
    x2 <- reactive(if (startsWith(j_nm(), "f_")) correct_freq(1 / x()) else x())

    # Choose profiling function for variable
    y <- reactive_profile_y(
      x2,
      dist_args = dist_args_p,
      n     = inputs$n$org,
      y_lbl = inputs$vars$y,
      i_nm  = i_nm,
      j_nm  = j_nm2
    )

    # Create data.table of results
    dt <- reactive(data.table(x = x(), y = y()))

    # Plot results
    output$plot <- renderHighchart(
      hchart(dt(), "line", hcaes(x = x, y = y))
    )
  })
}


reactive_profile_y <- function(x, dist_args, n, y_lbl, i_nm, j_nm) {
  reactive({
    if (i_nm() == "n") {
      profile_n(x(),
                dist_args = dist_args,
                y_lbl = y_lbl()
      )
    } else if (i_nm() == "test" && startsWith(j_nm(), "p_asymp")) {
      profile_test(x(),
                   dist_args = dist_args,
                   n     = n(),
                   y_lbl = y_lbl(),
                   j_nm  = j_nm()
      )()
    } else {
      profile_arg(x(),
                  dist_args = dist_args,
                  n     = n(),
                  y_lbl = y_lbl(),
                  i_nm  = i_nm(),
                  j_nm  = j_nm()
      )()
    }
  }, label = "profile_y()")
}


seq_profile <- function(x, n = 55, intervals = c(1, 2, 5, 10)) {
  if (NROW(x) == 1) return(x)
  # Ensure that x is length 2 numeric
  checkmate::assert_numeric(x, len = 2L, finite = TRUE, any.missing = FALSE)

  # Range
  r <- abs(diff(x))

  # Magnitude of intervals
  m <- floor(log10(r)) - floor(log10(n))

  # Scale candidate intervals
  intervals <- intervals * 10^m

  # Must yield `n` intervals or fewer; pick smallest from remaining
  by <- min(intervals[r / intervals <= n])

  # Create sequence; count down if x[[1]] is larger than x[[2]]
  s <- seq(x[[1]], x[[2]], by = if (x[[1]] > x[[2]]) -by else by)

  # Ensure endpoints are always included
  if (x[[2]] != s[[NROW(s)]]) c(s, x[[2]]) else s
}


freq_to_prob <- function(x) {
  p <- purrr::map_if(x, startsWith(names(x), "f_"), ~ correct_freq(1 / .x))
  names(p) <- stringr::str_replace(names(x), "^f_", "p_")
  p
}

## To be copied in the UI
# mod_profiling_output_ui("profiling_output_ui_1")

## To be copied in the server
# mod_profiling_output_server("profiling_output_ui_1")
