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
    navbarPage(h3("covidtest"), theme = ct_theme(), fluid = TRUE,
      bslib::nav_spacer(),
      # Home/landing page
      tabPanel("", icon = ct_h5(icon("home"), tooltip = "Home")),
      ct_nav_pad("3rem"),
      # Scenario calculation
      tabPanel(ct_h5("Scenarios", tooltip = "Explore your risks and the potential benefits of regular testing"),
               mod_scenarios_ui("scenarios")
      ),
      ct_nav_pad("3rem"),
      # Input profiling
      tabPanel(ct_h5("Inputs", tooltip = "See risks and benefits change across input values")),
      ct_nav_pad("3rem"),
      # Tutorial and explanation of use
      tabPanel(ct_h5("Tutorial", tooltip = "A walkthrough of covidtest functionality")),
      ct_nav_pad("3rem"),
      # FAQs
      tabPanel(ct_h5("FAQs", tooltip = "Frequently Asked Questions")),
      ct_nav_pad("3rem"),
      # More links
      navbarMenu("",
        icon = ct_h5(icon("external-link-alt"), tooltip = "External links"),
        align = "right",
        ct_nav_ext_link("Paper",
                        href = NULL,
                        icon = icon("file-alt")
        ),
        ct_nav_ext_link("Package",
                        href = "https://jesse-smith.github.io/covidtest",
                        icon = icon("r-project")
        ),
        ct_nav_ext_link("Source Code",
                        href = "https://github.com/jesse-smith/covidtest",
                        icon = icon("github")
        )
      ),
      bslib::nav_spacer()
    )
  )
}

#' Bootstrap Theme for {covidtest} Shiny App
#'
#' Wrapper for `bslib::bs_theme()` with options set to the covidtest theme.
#'
#' @return A `bs_theme`
#' @noRd
ct_theme <- function() {
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
    $(document).ready(function(){
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
      app_title = "covidtest"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
