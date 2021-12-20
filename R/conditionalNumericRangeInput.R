conditionalNumericRangeInput <- function(
  id,
  label,
  value,
  value2,
  condition,
  ns = NS(NULL),
  min = NA,
  max = NA,
  step = NA,
  sep = " to ",
  style = NULL
) {
  # Restore input
  value_rst <- shiny::restoreInput(id = id, default = NULL)
  if (!is.null(value_rst) && NROW(value_rst == 1)) {
    value <- value_rst
  } else if (!is.null(value_rst) && NROW(value_rst > 1)) {
    value2 <- range(value_rst, na.rm = TRUE)
  }

  # Create inverse `condition`
  not_condition <- paste0("!(", condition, ")")

  # Create input tags
  pointTag <- value[[1]] %>%
    num_input_tag() %>%
    set_numeric_input_attribs(min, max, step)
  fromTag <- value2[[1]] %>%
    num_input_tag("left") %>%
    set_numeric_input_attribs(min, max, step)
  toTag <- value2[[2]] %>%
    num_input_tag("right") %>%
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
    `data-display-if` = not_condition,
    `data-ns-prefix`  = ns(""),
    pointTag
  )
  rangeContainer <- tags$div(
    class = "input-group shiny-input-container",
    `data-display-if` = condition,
    `data-ns-prefix`  = ns(""),
    fromTag,
    sepTag,
    toTag
  )

  # Return UI element
  tags$div(
    id = id,
    class = "shiny-conditional-numeric-range-input input-group shiny-input-container",
    `data-range-condition` = jsonlite::toJSON(TRUE, auto_unbox = TRUE),
    style = style,
    shinyInputLabel(id, label),
    pointContainer,
    rangeContainer
  )
}

#' Change the value of a numeric range input
#'
#' @param session The session object passed to function given to shinyServer.
#' @inheritParams numericRangeInput
#'
#' @seealso [numericRangeInput()]
#'
#' @export
#'
#' @example examples/updateNumericRangeInput.R
updateConditionalNumericRangeInput <- function(
  id,
  label = NULL,
  value = NULL,
  session = getDefaultReactiveDomain()
  ) {

  if (!is.null(value) && NROW(value) > 1) value <- range(value, na.rm = TRUE)
  message <- list(label = label, value = value)
  session$sendInputMessage(id, drop_nulls(message))
}


choose_num_range <- function(input, id, choice_var) {
  reactive({
    if (input[[choice_var]] == id) {
      input[[id]][2:3]
    } else {
      input[[id]][[1]]
    }
  })
}


# Format values for Shiny numeric inputs
formatNoSci <- function(x) {
  if (is.null(x)) NULL else format(x, scientific = FALSE, digits = 15)
}


# Convenience function for creating inputs
num_input_tag <- function(
  value,
  side = c("none", "left", "right"),
  condition = NULL,
  ns = NS(NULL)
) {
  value_fmt <- formatNoSci(value)
  side <- rlang::arg_match(side)[[1]]
  class <- paste0(
    "form-control",
    if (side != "none") paste0(" rounded-", side)
  )
  if (is.null(condition) || is.na(condition)) {
    input <- tags$input(
      type  = "number",
      class = class,
      placeholder = value_fmt,
      value = value_fmt
    )
  } else {
    input <- tags$input(
      type  = "number",
      class = class,
      `data-display-if` = condition,
      `data-ns-prefix`  = ns(""),
      placeholder = value_fmt,
      value = value_fmt
    )
  }

  input
}


# Convenience function for setting attributes
set_numeric_input_attribs <- function(input, min = NA, max = NA, step = NA) {
  if (!(is.na(min) || !is.null(min)))   input$attribs$min  <- min[[1]]
  if (!(is.na(max) || !is.null(max)))   input$attribs$max  <- max[[1]]
  if (!(is.na(step) || !is.null(step))) input$attribs$step <- step[[1]]
  input
}


# Create a Shiny input label
shinyInputLabel <- function(id, label = NULL) {
  tags$label(
    label,
    class = "control-label",
    class = if (is.null(label)) "shiny-label-null",
    id = paste0(id, "-label"),
    `for` = id
  )
}
