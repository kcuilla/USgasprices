#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  output$title <- typedjs::renderTyped({
    typedjs::typed(c("U.S. Gas Prices^600 <br> <span style='font-style: italic;'>Visualized</span>",
                     "Data from:^600 <br> <span style='font-style: italic;'>EIA</span>",
                     "Created by:^600 <br> <span style='font-style: italic;'>Kyle Cuilla</span>"),
                     typeSpeed = 28)
  })

  callModule(mod_table_server, "table")
  callModule(mod_chart_server, "linechart")

}
