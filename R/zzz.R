.onLoad <- function(...) {
  shiny::registerInputHandler(
    "covidtest.conditionalRangeInput", function(x, session, name){
      # Unlist values
      x_out <- list(point = x$values$point, range = unlist(x$values$range))
      # Set attributes
      attr(x_out, "id")      <- x$attributes$inputId
      attr(x_out, "trigger") <- x$attributes$trigger
      # Return
      x_out
    },
    force = TRUE
  )
}

.onUnload <- function(...) {
  shiny::removeInputHandler("covidtest.conditionalRangeInput")
}
