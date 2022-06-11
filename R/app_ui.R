#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
# app_ui <- function(request) {
#   tagList(
#     # Leave this function for adding external resources
#     golem_add_external_resources(),
#     # Your application UI logic
#     fluidPage(
#       h1("gasprices")
#     )
#   )
# }
app_ui <- function() {

  # options <- list()

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fullPage::pagePiling(
      sections.color = c('#ffffff', "#ffffff", '#ffffff', "#ffffff"),
      # opts = options,
      menu = c(
        "Home" = "home",
        "Latest Trends" = "table",
        "Historical Trends" = "chart",
        "About" = "about"
      ),
      fullPage::pageSectionImage(
        center = TRUE,
        img = "www/img/refinery.jpg",
        menu = "home",
        h1(typedjs::typedOutput("title"), class = "intro")
      ),
      fullPage::pageSection(
        center = TRUE,
        menu = "table",
        mod_table_ui("table")
      ),
      fullPage::pageSection(
        center = TRUE,
        menu = "chart",
        mod_chart_ui("linechart")
      ),
      fullPage::pageSectionImage(
        center = TRUE,
        img = "www/img/keyboard.jpg",
        menu = "about",
        tags$p(class = "about-header shift-up",
          HTML(paste0(
            "Data sourced from the U.S. Energy Information Administration ",
            "<a href='https://www.eia.gov/petroleum/gasdiesel/' target='_blank' class='web-link'>(EIA)</a>"))
        ),
        h3(class = "light shift-up", tags$i("Data refreshed once per week every Monday")),
        h1(
          class = "shift-up",
          tags$a("Code", href = "https://github.com/news-r/fopi.app", target = "_blank", class = "code-link")
        ),
        tags$p(class = "footer",
          HTML(paste0(
            "<a href='https://uncharteddata.netlify.app/' target='_blank'><i class='fas fa-globe fa-2x' style='color:#ffffff; padding:6px;'></i></a>",
            "<a href='https://www.linkedin.com/in/kylecuilla/' target='_blank'><i class='fab fa-linkedin-in fa-2x' style='color:#ffffff; padding:6px;'></i></a>",
            "<a href='https://www.twitter.com/kc_analytics' target='_blank'><i class='fab fa-twitter fa-2x' style='color:#ffffff; padding:6px;'></i></a>",
            "<a href='https://github.com/kcuilla' target='_blank'><i class='fab fa-github fa-2x' style='color:#ffffff; padding:6px;'></i></a>"))
        )
      )
    )
  )
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

  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$link(rel = "stylesheet", type = "text/css", href = "www/css/style.css"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "gasprices"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
