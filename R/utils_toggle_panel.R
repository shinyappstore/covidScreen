#' Create a toggle panel in the UI
#'
#' A toggle panel is a panel with show/hide functionality by clicking on the
#' title and additional info by clicking on an info button. `toggle_panel_ui()`
#' creates the UI definition for the panel.
#'
#' @param id The UI element ID (technically the ID of the title)
#' @param ... Panel content
#' @param label The panel title
#' @param icon An icon to display to the right of the title
#' @param tag_fn A tag to wrap the label and icon
#' @param init_visible Should the panel initialize as visible or invisible?
#'
#' @return A UI element
toggle_panel_ui <- function(
  id,
  ...,
  label = NULL,
  icon = NULL,
  tag_fn = NULL,
  init_visible = TRUE
) {
  panel <- tags$div(id = paste0(id, "_panel"), ...)
  tags$div(
    action_link(id, label = label, icon = icon, tag_fn = tag_fn),
    if (init_visible) panel else shinyjs::hidden(panel)
  )
}


#' Handle a toggle panel in the Server
#'
#' A toggle panel is a panel with show/hide functionality by clicking on the
#' title and additional info by clicking on an info button.
#' `toggle_panel_server()` defines the logic for show/hide and click events.
#'
#' @param id The element ID
#' @param input The shiny session input
#' @inheritParams shinyjs::toggle
#'
#' @return The return value of `bindEvent()`
toggle_panel_server <- function(
  id,
  input,
  anim = TRUE,
  animType = "slide",
  time = 0.5
) {
  bindEvent(
    observe(shinyjs::toggle(paste0(id, "_panel"),
                            anim = anim,
                            animType = animType,
                            time = time
    )),
    input[[id]]
  )
}
