#' Numeric Input for Point & Range
#' 
#' Displays and returns a numeric
#' scalar or range depending on the
#' current value of `condition`
#'
#' @param id The input id
#' @param label A label for the input UI
#' @param value A single number to initalize the point input
#' @param value2 A length 2 numeric vector to initialize the range input
#' @param condition Same as in `conditionalPanel()`
#' @param ns A namespacing function; needed when used inside modules
#' @param min The minimum value
#' @param max The maximum value
#' @param step The input step size
#' @param sep A separator for the range input boxes
#' @param style A style attribute for the input element
#'
#' @return A UI input element
numericInput2 <- function(
  id,
  label,
  value,
  value2,
  condition,
  ns = NS(NULL),
  min = NA,
  max = NA,
  step = NA,
  sep = "to",
  style = htmltools::css(width = "240px")
) {
  # Restore inputs
  value_rst <- restoreInput(id, NULL)
  if (NROW(value_rst) == 1) {
    value  <- value_rst
  } else if (NROW(value_rst) == 2) {
    value2 <- value_rst
  }
  
  # Invert condition
  not_condition <- paste0("!(", condition, ")")
  
  # Create input tags
  pntTag <- value %>%
    num_input_tag() %>%
    set_numeric_input_attribs(min, max, step)
  rngTag1 <- value2[[1]] %>%
    num_input_tag() %>%
    set_numeric_input_attribs(min, max, step)
  rngTag2 <- value2[[2]] %>%
    num_input_tag() %>%
    set_numeric_input_attribs(min, max, step)
  # Create separator tag
  if (is.null(sep) || is.na(sep)) {
    sepTag <- NULL
  } else {
    sepTag <- tags$span(
      style = htmltools::css(
        `margin-left`  = "0.5rem",
        `margin-right` = "0.5rem"
      ),
      "to"
    )
  }
  
  # Create input containers
  pointTag <- tags$div(
    class = "input-group shiny-input-container",
    `data-display-if` = not_condition,
    `data-ns-prefix`  = ns,
    pntTag
  )
  rangeTag <- tags$div(
    class = "input-group shiny-input-container",
    `data-display-if` = condition,
    `data-ns-prefix`  = ns,
    rngTag1,
    sepTag,
    rngTag2
  )
  
  # Return input
  tags$div(
    id = id,
    class = "shiny-numeric-input-2 shiny-input-container",
    style = style,
    shinyInputLabel(id, label),
    pointTag,
    rangeTag
  )
}


num_input_tag <- function(value) {
  value_fmt <- formatNoSci(value)
  tags$input(
    type  = "number",
    class = "form-control",
    placeholder = value_fmt,
    value = value_fmt
  )
}


set_numeric_input_attribs <- function(input, min = NA, max = NA, step = NA) {
  if (!is.na(min)) input$attribs$min <- min
  if (!is.na(max)) input$attribs$max <- max
  if (!is.na(step)) input$attribs$step <- step
  input
}


formatNoSci <- function(x) {
  if (is.null(x)) NULL else format(x, scientific = FALSE, digits = 15)
}


shinyInputLabel <- function(id, label = NULL) {
  tags$label(label,
             class = "control-label",
             class = if (is.null(label)) "shiny-label-null",
             id = paste0(id, "-label"),
             `for` = id
             )
}
