#' HTML Headers with tooltips
#'
#' Shiny `tags$h()` functions with tooltips
#'
#' @param ... Inputs to the header function
#' @param tooltip Tooltip text
#'
#' @return A `tag` wrapped in a `div` element
#'
#' @name tag_tooltip
NULL

#' @rdname tag_tooltip
cs_h1 <- function(..., tooltip = NULL) {
  cs_tooltip(tags$h1(...), tooltip = tooltip)
}

#' @rdname tag_tooltip
cs_h2 <- function(..., tooltip = NULL) {
  cs_tooltip(tags$h2(...), tooltip = tooltip)
}

#' @rdname tag_tooltip
cs_h3 <- function(..., tooltip = NULL) {
  cs_tooltip(tags$h3(...), tooltip = tooltip)
}

#' @rdname tag_tooltip
cs_h4 <- function(..., tooltip = NULL) {
  cs_tooltip(tags$h4(...), tooltip = tooltip)
}

#' @rdname tag_tooltip
cs_h5 <- function(..., tooltip = NULL) {
  cs_tooltip(tags$h5(...), tooltip = tooltip)
}

#' @rdname tag_tooltip
cs_h6 <- function(..., tooltip = NULL) {
  cs_tooltip(tags$h6(...), tooltip = tooltip)
}

#' Add a tooltip to an HTML Element from `tags`
#'
#' @param ... Elements to pass to `tags$div()`
#' @param tooltip Tooltip text - passed to the `title` of `tags$div()`
#'
#' @return A `div` element
cs_tooltip <- function(..., tooltip = NULL) {
  tags$div(title = tooltip, ...)
}
