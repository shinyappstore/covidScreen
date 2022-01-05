#' Update Argument in a reactiveValues dist_args Object
#'
#' `insert_args()` updates a reactiveValues object containing arguments for
#' `reactive_dist()` without overwriting values elsewhere. It can either update
#' an entire sublist or a selected vector within a sub-list.
#'
#' @param new_arg The replacement for the argument to be updated
#' @param dist_args Existing arguments for `calc_dist()`
#' @param i_nm The name of the sub-list to be updated
#' @param j_nm The name of the specific vector to be updated, if any
#'
#' @return An updated reactiveValues object
insert_args <- function(
  new_arg,
  dist_args,
  i_nm = c("vac", "inf", "symp", "test", "detect"),
  j_nm = NULL
) {
  # Check sub-list name validity
  checkmate::assert_choice(
    i_nm,
    choices = c("vac", "inf", "symp", "test", "detect")
  )
  # Create a new reactiveValues() object
  new_args <- reactiveValues()
  # Pass the unselected sub-list on directly (by reference)
  if (i_nm != "vac") {
    new_args$vac <- reactive(dist_args$vac())
  }
  if (i_nm != "inf") {
    new_args$inf <- reactive(dist_args$inf())
  }
  if (i_nm != "symp") {
    new_args$symp <- reactive(dist_args$symp())
  }
  if (i_nm != "test") {
    new_args$test <- reactive(dist_args$test())
  }
  if (i_nm != "detect") {
    new_args$detect <- reactive(dist_args$detect())
  }

  # Update the sub-list or vector
  if (is.null(j_nm)) {
    new_args[[i_nm]] <- new_arg
  } else {
    new_i <- reactive(
      magrittr::inset2(dist_args[[i_nm]](), j_nm, new_arg),
      label = "insert_j_arg()"
    )

    new_args[[i_nm]] <- new_i
  }

  # Return the new object
  new_args
}


const_testing <- function(dist_args, p_vac = 0, p_unvac = 0) {
  if (is.reactivevalues(dist_args)) {
    c_test_args <- reactiveValues(
      n = dist_args$n,
      vac = dist_args$vac,
      inf = dist_args$inf,
      symp = dist_args$symp,
      detect = dist_args$detect
    )

    c_test_args$test <- reactive(list(
      p_symp = dist_args$test()$p_symp,
      p_asymp_vac = p_vac,
      p_asymp_unvac = p_unvac
    ))
  } else {
    c_test_args <- dist_args
    c_test_args$test$p_asymp_vac   <- p_vac
    c_test_args$test$p_asymp_unvac <- p_unvac
  }

  c_test_args
}
