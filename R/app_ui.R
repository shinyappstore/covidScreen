#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Set up shinyjs
    shinyjs::useShinyjs(),
    # Set up other JS
    head_js(),
    # UI definition
    navbarPage(h3("covidscreen"),
      theme = cs_theme(), fluid = TRUE, selected = "scenarios",
      bslib::nav_spacer(),
      # Home/landing page
      tab_home(),
      cs_nav_pad("3rem"),
      # Scenario calculation
      tab_scenarios(),
      cs_nav_pad("3rem"),
      # Input profiling
      tab_profiling(),
      cs_nav_pad("3rem"),
      # FAQs
      tab_faq(),
      cs_nav_pad("3rem"),
      # More links
      nav_menu(),
      bslib::nav_spacer()
    )
  )
}

#' Define Tabs in App
#'
#' @return A `tabPanel` or `navbarMenu`
#'
#' @keywords internal
#'
#' @name tabs

#' @rdname tabs
tab_home <- function() {
  tabPanel("", value = "home", icon = cs_h5(icon("home"), tooltip = "Home"))
}


#' @rdname tabs
tab_scenarios <- function() {
  tt <- "Explore infection risks and potential benefits of regular testing"
  tabPanel(
    cs_h5("Scenarios", tooltip = tt),
    value = "scenarios",
    mod_scenarios_ui("scenarios")
  )
}


#' @rdname tabs
tab_profiling <- function() {
  tt <- "See risks and benefits change across input values"
  tabPanel(
    cs_h5("Profiling", tooltip = tt),
    value = "profiling",
    mod_profiling_ui("profiling")
  )
}


#' @rdname tabs
tab_faq <- function() {
  tabPanel(cs_h5("FAQs", tooltip = "Frequently Asked Questions"))
}


#' @rdname tabs
nav_menu <- function() {
  # Links in menu
  paper_link <- cs_nav_ext_link("Paper", href = NULL, icon = icon("file-alt"))
  pkg_link   <- cs_nav_ext_link(
    "Package",
    href = "https://jesse-smith.github.io/covidscreen",
    icon = icon("r-project")
  )
  code_link  <- cs_nav_ext_link(
    "Source Code",
    href = "https://github.com/jesse-smith/covidscreen",
    icon = icon("github")
  )
  # Icon
  menu_icon <- cs_h5(icon("external-link-alt"), tooltip = "External links")
  # Create menu
  navbarMenu(
    "", icon = menu_icon, align = "right",
    paper_link,
    pkg_link,
    code_link
  )
}


#' Bootstrap Theme for {covidscreen} Shiny App
#'
#' Wrapper for `bslib::bs_theme()` with options set to the covidscreen theme.
#'
#' @return A `bs_theme`
#' @noRd
cs_theme <- function() {
  bslib::bs_theme(
    bootswatch = "litera",
    fg = "#29434e", bg = "#ffffff",
    primary = "#29434e", secondary = "#29434e",
    success = "#98ee99", info = "#e0e0e0",
    warning = "#ffa726", danger = "#b61827",
    base_font = bslib::font_google("Roboto"),
    code_font = bslib::font_google("Roboto Mono")
  )
}

head_js <- function() {
  tags$head(HTML(JS("
    <script>
    $(document).ready(function() {
      $('[data-toggle=\"tooltip\"]').tooltip();
    });
    </script>
  ")))
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {

  add_resource_path("www", app_sys("app/www"))

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "covidscreen"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
