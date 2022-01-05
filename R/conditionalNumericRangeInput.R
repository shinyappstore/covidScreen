#' Display a Point or Range Input Box Based on a Trigger Input
#'
#' This input will display either a single input box or a range input, depending
#' on whether it is selected in a trigger variable. It always returns both the
#' point and the range; use `chooseConditionalRange()` to choose the one
#' currently displayed in the UI.
#'
#' @param id The element ID
#' @param label A label for the UI element
#' @param value `[numeric(1)]` The initial "point" value
#' @param value2 `[numeric(2)]` The initial "range" value
#' @param trigger `[character(1)]` The variable that triggers the switch between
#'   point and range
#' @param ns The session namespace
#' @param min The minimum allowable value of the inputs
#' @param max The maximum allowable value of the inputs
#' @param step The step size
#' @param sep The separator between the range input boxes
#'
#' @return A UI element
conditionalNumericRangeInput <- function(
  id,
  label,
  value,
  value2,
  trigger,
  ns = NS(NULL),
  min = NA,
  max = NA,
  step = NA,
  sep = " to "
) {
  # Restore input
  value_rst <- shiny::restoreInput(id = id, default = NULL)
  if (!is.null(value_rst) && NROW(value_rst == 1)) {
    value <- value_rst
  } else if (!is.null(value_rst) && NROW(value_rst > 1)) {
    value2 <- range(value_rst, na.rm = TRUE)
  }

  # Create conditions
  id_chr <- stringr::str_remove(id, stringr::fixed(ns("")))
  condition     <- paste0("input.", trigger, " == '", id_chr, "'")
  not_condition <- paste0("input.", trigger, " != '", id_chr, "'")

  # Create input tags
  pointTag <- value[[1]] %>%
    num_input_tag(id = paste0(id, "-point")) %>%
    set_numeric_input_attribs(min, max, step)
  fromTag <- value2[[1]] %>%
    num_input_tag("From", id = paste0(id, "-range-1")) %>%
    set_numeric_input_attribs(min, max, step)
  toTag <- value2[[2]] %>%
    num_input_tag("To", id = paste0(id, "-range-2")) %>%
    set_numeric_input_attribs(min, max, step)

  # Create separator tag
  sepTag <- tags$span(
    class = "input-group-addon input-group-text rounded-0 text-muted bg-light",
    style = htmltools::css(
      `border-left`  = "none",
      `border-right` = "none"
    ),
    sep
  )

  # Create input containers
  pointContainer <- tags$div(
    class = "input-group shiny-input-container",
    shinyInputLabel(id, label),
    `data-display-if` = not_condition,
    `data-ns-prefix`  = ns(""),
    tags$div(
      class = "input-group form-group",
      pointTag
    )
  )
  rangeContainer <- tags$div(
    class = "input-group shiny-input-container",
    `data-display-if` = condition,
    `data-ns-prefix`  = ns(""),
    tags$b(shinyInputLabel(id, label)),
    tags$div(
      class = "input-group form-group",
      tags$span(
        style = "width: 4.5em",
        class = "input-group-prepend input-group-text",
        "From"
      ),
      fromTag
    ),
    tags$div(
      class = "input-group form-group",
      tags$span(
        style = "width: 4.5em",
        class = "input-group-prepend input-group-text",
        "To"
      ),
      toTag
    )
  )

  # Return UI element
  tags$div(
    id = id,
    class = paste(
      "shiny-conditional-numeric-range-input",
      "input-group",
      "shiny-input-container"
    ),
    `data-condition-variable` = trigger,
    `data-ns-prefix` = ns(""),
    pointContainer,
    rangeContainer
  )
}


#' Change the Value or Label of a conditionalNumericRangeInput
#'
#' Sends a message to the client to update the either `value`, `value2`,
#' or `label` in the `conditionalNumericRangeInput()`
#'
#' @inheritParams conditionalNumericRangeInput
#' @param session The shiny `session` object; the default is usually appropriate
#'
#' @return The output of `session$sendInputMessage()`
updateConditionalNumericRangeInput <- function(
  id,
  label = NULL,
  value = NULL,
  value2 = NULL,
  session = getDefaultReactiveDomain()
  ) {

  if (!is.null(value) && NROW(value) > 1) value <- range(value, na.rm = TRUE)
  message <- list(label = label, value = value)
  session$sendInputMessage(id, drop_nulls(message))
}


#' Statically elect the Point or Range from a Conditional Range Input
#'
#' Use this function to choose the point or range from either a
#' `conditionalNumericRangeInput()` or `conditionalSliderRangeInput()`. It
#' finds the trigger dependency and choose the value accordingly.
#'
#' @param x The value from a conditional range input
#' @param session The Shiny `session`; the default is usually appropriate
#'
#' @return Either a length 1 ("point") or length 2 ("range") numeric
selectPointRange <- function(x, session = getDefaultReactiveDomain()) {
  trigger <- attr(x, "trigger")
  id      <- attr(x, "id")
  isolate(if (session$input[[trigger]] == id) x$range else x$point)
}


#' Reactively Select the Point or Range from a Conditional Range Input
#'
#' Use this function to select the point or range value from a
#' `conditionalNumericRangeInput()` or a `conditionalSliderRangeInput()`. It
#' finds the trigger dependency and selects the appropriate value. Unlike
#' `selectPointRange()`, this returns a reactive expression.
#'
#' @param id The ID of the input
#' @inheritParams selectPointRange
#'
#' @return A reactive expression containing the chose input value
reactivePointRange <- function(id, session = getDefaultReactiveDomain()) {
  trigger <- isolate(attr(session$input[[id]], "trigger"))
  reactive({
    x <- session$input[[id]]
    if (session$input[[trigger]] == id) x$range else x$point
  }, label = paste0("range_if(", id, ")"))
}


# Helpers ----------------------------------------------------------------------


# Convenience function for creating inputs
num_input_tag <- function(
  value,
  placeholder = "e.g. {value}",
  id = NULL,
  width = "120px"
) {
  value <- formatNoSci(value)
  tags$input(
    id = id,
    type  = "number",
    class = "form-control",
    style = htmltools::css(width = validateCssUnit(width)),
    placeholder = glue::glue(placeholder),
    value = value
  )
}


# Format values for Shiny numeric inputs
formatNoSci <- function(x) {
  if (is.null(x)) NULL else format(x, scientific = FALSE, digits = 15)
}


# Convenience function for setting attributes
set_numeric_input_attribs <- function(input, min = NA, max = NA, step = NA) {
  if (!(is.na(min)  || !is.null(min)))   input$attribs$min  <- min[[1]]
  if (!(is.na(max)  || !is.null(max)))   input$attribs$max  <- max[[1]]
  if (!(is.na(step) || !is.null(step)))  input$attribs$step <- step[[1]]
  input
}


# Create a Shiny input label
shinyInputLabel <- function(id, label = NULL) {
  tags$label(
    label,
    class = "control-label",
    class = if (is.null(label)) "shiny-label-null",
    `for` = id
  )
}
