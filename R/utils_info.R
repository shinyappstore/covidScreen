#' Add Info Modal to UI
#'
#' `info_ui()` adds an `actionLink()` to the that brings up the modal in
#' `info_server()`.
#'
#' @param id The input ID
#' @param label The text to display with the info link, if any
#' @param icon The icon to display on the info link
#' @param container A container to wrap the `actionLink`, if any. Defaults to
#'   `tags$sup()`.
#' @param ... UI elements for the body of the modal dialog box
#' @param title An optional title for the dialog
#' @param footer UI for the footer. Use `NULL` for no footer.
#' @param size One of `"s"` for small, `"m"` for medium, `"l"` for large, or
#'   `"xl"` for extra large
#' @param easyClose Can the modal dialog be dismissed by clicking outside the
#'   dialog box? If `FALSE`, the modal dialog must be dismissed by clicking on
#'   a `modalButton()` or from a call to `removeModal()` on the server.
#' @param fade If `FALSE`, the modal dialog will have no fade-in animation
#'
#' @name info_modal
NULL

#' @rdname info_modal
ct_info_ui <- function(
  id,
  label = NULL,
  icon = shiny::icon("info-circle"),
  container = tags$sup
) {
  container(actionLink(id, label = label, icon = icon))
}

#' @rdname info_modal
ct_info_server <- function(
  ...,
  title = NULL,
  footer = modalButton("Dismiss"),
  size = c("m", "s", "l", "xl"),
  easyClose = TRUE,
  fade = TRUE
) {
  showModal(modalDialog(
    ..., title = title, footer = footer,
    size = rlang::arg_match(size)[[1]], easyClose = easyClose, fade = fade
  ))
}
