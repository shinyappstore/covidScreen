#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
#' @importFrom magrittr %>%
## usethis namespace: end
NULL

#' {highcharter} for Interactive Plotting
#'
#' All functions are prefixed with `hc_` to avoid namespace collisions
#'
#' @import highcharter
NULL

# Suppress R CMD CHECK NOTE on `.data`
if (getRversion() >= "2.15.1") utils::globalVariables(c(".data", "."))
