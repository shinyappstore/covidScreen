#' Add a Hyper-linked Item to a Navbar or Navbar Menu
#'
#' @param title The title of the item
#' @param href The URL to link to
#' @param icon An optional icon for the item
#'
#' @return A `bslib::nav_item`
ct_nav_ext_link <- function(title, href, icon = NULL) {
  bslib::nav_item(tags$a(href = href, tags$span(icon, title)))
}

#' Add Padding to a Navbar or Navbar Menu
#'
#' @param width The width of the padding element (a `span`)
#'
#' @return A `tags$span()` element with `style = paste("width:", width)`
ct_nav_pad <- function(width = "1rem") {
  bslib::nav_item(tags$span(style = paste("width:", width)))
}
