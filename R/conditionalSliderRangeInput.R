#' Display a Point or Range Slider Based on a Trigger Input
#'
#' This input will display a slider with either a single or a double handle,
#' depending on whether it is selected in a trigger variable. It always returns
#' both the point (single) and the range (double) values;
#' use `choosePointRange()` to statically choose the one currently
#' displayed in the UI, or `reactivePointRange()` to choose reactively.
#'
#' @param id The input element ID
#' @param label The input element label
#' @param value `[numeric(1)]` The point value for the single handle
#' @param value2 `[numeric(2)]` The range of the double handles
#' @param trigger `[character(1)]` The trigger variable
#' @param ns The shiny session namespace
#' @param min The minimum slider value
#' @param max The maximum slider value
#' @param step The step size of the slider
#' @param prefix A prefix to prepend to the value labels
#' @param suffix A suffix to append to the value labels
#'
#' @return A UI element
conditionalSliderRangeInput <- function(
  id,
  label,
  value,
  value2,
  trigger,
  ns = NS(NULL),
  min = NA,
  max = NA,
  step = NA,
  prefix = NULL,
  suffix = NULL
) {
  # Restore input
  value_rst <- shiny::restoreInput(id = id, default = NULL)
  if (!is.null(value_rst) && NROW(value_rst == 1)) {
    value <- value_rst
  } else if (!is.null(value_rst) && NROW(value_rst > 1)) {
    value2 <- range(value_rst, na.rm = TRUE)
  }

  # Set `min`, `max`, and `step` if not already
  if (is.null(min) || is.na(min)) min <- base::min(value2, value, na.rm = TRUE)
  if (is.null(max) || is.na(max)) max <- base::max(value2, value, na.rm = TRUE)
  if (is.null(step) || is.na(step)) step <- findStepSize(step, min, max)

  # Create conditions
  id_chr <- stringr::str_remove(id, stringr::fixed(ns("")))
  condition     <- paste0("input.", trigger, " == '", id_chr, "'")
  not_condition <- paste0("input.", trigger, " != '", id_chr, "'")

  # Create input tags
  pointTag <- slider_input_tag(
    paste0(id, "-point"),
    value = value,
    min = min,
    max = max,
    step = step,
    prefix = prefix,
    suffix = suffix
  )
  rangeTag <- slider_input_tag(
    paste0(id, "-range"),
    value = value2,
    min = min,
    max = max,
    step = step,
    prefix = prefix,
    suffix = suffix
  )

  # Create input containers
  pointContainer <- tags$div(
    class = "shiny-input-container",
    shinyInputLabel(id, label),
    `data-display-if` = not_condition,
    `data-ns-prefix`  = ns(""),
    tags$div(
      class = "form-group",
      pointTag
    )
  )
  rangeContainer <- tags$div(
    class = "shiny-input-container",
    tags$b(shinyInputLabel(id, label)),
    `data-display-if` = condition,
    `data-ns-prefix`  = ns(""),
    tags$div(
      class = "form-group",
      rangeTag
    )
  )

  # Return UI element
  tags$div(
    id = id,
    class = paste(
      "shiny-conditional-slider-range-input",
      "input-group",
      "shiny-input-container"
    ),
    `data-condition-variable` = trigger,
    `data-ns-prefix` = ns(""),
    pointContainer,
    rangeContainer
  )
}


# Helpers ----------------------------------------------------------------------


# Create an ionRangeSlider input tag
slider_input_tag <- function(
  id,
  value,
  min,
  max,
  step,
  prefix,
  suffix
) {
  # Create the slider using `sliderInput()`; just get input tag
  input <- sliderInput(
    inputId = id,
    label = NULL,
    min = min,
    max = max,
    value = value,
    step = step,
    ticks = FALSE,
    pre = prefix,
    post = suffix
  )$children[[2]]
  # Change skin to flat
  input$attribs$`data-skin` <- "flat"
  # Return
  input
}


# Find step size for slider if none given
findStepSize <- function(
  step,
  min,
  max,
  n = 220,
  intervals = c(0.5, 1, 2, 5, 10)
) {
  # Return step if provided
  if (!(is.null(step) || is.na(step))) return(step)

  # Range
  r <- base::max(max, na.rm = TRUE) - base::min(min, na.rm = TRUE)

  # Magnitude of intervals
  m <- floor(log10(r)) - floor(log10(n))

  # Scale candidate intervals
  intervals <- intervals * 10^m

  # Must yield `n` intervals or fewer; pick smallest from remaining
  min(intervals[r / intervals <= n])
}
