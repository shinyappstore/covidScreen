numericInput2 <- function(
  id,
  value,
  value2,
  condition,
  label = NULL,
  min = NA,
  max = NA,
  step = NA,
  sep = NA,
  style = htmltools::css(width = "240px")
) {
  # Get namespace, if it exists
  id_quo <- rlang::enquo(id)
  ns <- if (rlang::quo_is_call(id_quo)) rlang::call_fn(id_quo) else NS(NULL)

  # Split into individual ids

  value <- restoreInput(id, value)[[1]]
  value2 <- restoreInput(id, value2)
  if (NROW(value2) == 1) value2 <- rep(value2, 2)

  condition_sngl <- paste0("!(", condition, ")")

  snglTag <- tags$input(type = "number",
                        placeholder = as.character(value),
                        class = "form-control",
                        `data-display-if` = condition_sngl,
                        `data-ns-prefix`  = ns(""),
                        value = formatNoSci(value))
  fromTag <- tags$input(type = "number",
                        placeholder = as.character(value2[[1]]),
                        class = "form-control",
                        `data-display-if` = condition,
                        `data-ns-prefix`  = ns(""),
                        value = formatNoSci(value2[[1]]))
  toTag <- tags$input(type = "number",
                      placeholder = as.character(value2[[2]]),
                      class = "form-control",
                      `data-display-if` = condition,
                      `data-ns-prefix`  = ns(""),
                      value = formatNoSci(value2[[2]]))

  sep <- tags$span(
    style = htmltools::css(`margin-left` = "0.5rem", `margin-right` = "0.5rem"),
    `data-display-if` = condition,
    `data-ns-prefix`  = ns(""),
    "to"
  )

  snglTag <- set_numeric_input_attribs(snglTag, min, max, step)
  fromTag  <- set_numeric_input_attribs(fromTag, min, max, step)
  toTag    <- set_numeric_input_attribs(toTag, min, max, step)

  tags$div(
    class = "shiny-numeric-input-2 input-group shiny-input-container",
    style = style,
    shinyInputLabel(label),
    snglTag,
    fromTag,
    sep,
    toTag
  )
}


set_numeric_input_attribs <- function(input, min, max, step) {
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
