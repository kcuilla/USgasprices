#' mod_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_table_ui <- function(id){
  ns <- NS(id)
  fullPage::pageContainer(
    fluidRow(
      br(),br(),br(),
      shinyWidgets::radioGroupButtons(
        inputId = ns("type"),
        label = h3("Select Gas Type:"),
        choices = c("Regular", "Midgrade", "Premium"),
        individual = TRUE,
        size = "lg",
        checkIcon = list(
          yes = icon("gas-pump")
        )
      )
    ),
    br(),
    fluidRow(
      column(
        width = 4, offset = 8,
      shinyWidgets::prettyRadioButtons(
        inputId = ns("change"),
        label = NULL,
        choiceNames = c("Change ($)", "Change (%)"),
        choiceValues = c("dollar", "percent"),
        inline = TRUE,
        fill = TRUE,
        thick = TRUE,
        status = "success",
        animation = "pulse"
      ))
    ),
    # br(),
    reactable::reactableOutput(ns("table"), height="60vh"),
    uiOutput(ns("desc"))
  )
}

#' mod_table Server Functions
#'
#' @noRd

mod_table_server <- function(input, output, session){
  ns <- session$ns

  output$desc <- renderUI({
    msg <- paste0("As of: ", format(max(gasprices::summary_table$updated), "%b %d, %Y"))
    h4(msg)
  })

  max_price <- max(gasprices::summary_table$value)

  output$table <- reactable::renderReactable({

    change_format <- switch(input$change,
                            "dollar" = scales::dollar_format(),
                            "percent" = scales::percent_format(accuracy = 0.1)
                            )

    gasprices::summary_table %>%
      dplyr::filter(type == input$type) %>%
      dplyr::select(1:7, dplyr::starts_with(input$change)) %>%
      dplyr::rename(wow = 8, mom = 9, yoy = 10) %>%
      dplyr::mutate(fill_colors = dplyr::case_when(
        location == "U.S." ~ "#7c7c7c",
        TRUE ~ "#f27405"
      )) %>%
    reactable::reactable(
      theme = reactablefmtr::no_lines(
        background_color = "#FFFFFF",
        font_size = 18,
        header_font_size = 16
      ),
      defaultSortOrder = "desc",
      defaultSorted = "value",
      pagination = FALSE,
      style = list(fontFamily = "Roboto, sans-serif"),
      # columnGroups = list(
      #   reactable::colGroup(name = "CHANGE FROM", columns = c("wow","mom","yoy"))
      # ),
      defaultColDef = reactable::colDef(headerVAlign = "bottom", html = TRUE),
      columns = list(
        type = reactable::colDef(show = FALSE),
        updated = reactable::colDef(show = FALSE),
        max_date = reactable::colDef(show = FALSE),
        date = reactable::colDef(show = FALSE),
        fill_colors = reactable::colDef(show = FALSE),
        location = reactable::colDef(name = "LOCATION"),
        max_value = reactable::colDef(name = "HIGHEST RECORDED", align = "center",
          cell = reactablefmtr::merge_column(.,
                                             spacing = 5,
                                             size = 18,
                                             merged_size = 18,
                                             merged_name = "max_date",
                                             merged_position = "right",
                                             merged_style = "italic")
        ),
        value = reactable::colDef(name = "DOLLARS PER GALLON", align = "center",
          cell = reactablefmtr::data_bars(.,
                                          background = "transparent",
                                          text_position = "center",
                                          animation = "width 0.4s linear",
                                          bar_height = 26,
                                          max_value = max_price,
                                          fill_color_ref = "fill_colors",
                                          number_fmt = scales::dollar_format(accuracy = 0.01))
        ),
        wow = reactable::colDef(name = "VS <br> LAST WEEK", align = "center", maxWidth = 125, #headerVAlign = "bottom", html = TRUE,
                                style = list(background = "rgba(0, 0, 0, 0.03)"),
          cell = reactablefmtr::icon_trend_indicator(.,
                                                     icons = "angle-double",
                                                     colors = c("darkgreen","grey","red"),
                                                     number_fmt = change_format)
        ),
        mom = reactable::colDef(name = "VS <br> LAST MONTH", align = "center", maxWidth = 125, #headerVAlign = "bottom", html = TRUE,
                                style = list(background = "rgba(0, 0, 0, 0.03)"),
          cell = reactablefmtr::icon_trend_indicator(.,
                                                     icons = "angle-double",
                                                     colors = c("darkgreen","grey","red"),
                                                     number_fmt = change_format)
        ),
        yoy = reactable::colDef(name = "VS <br> LAST YEAR", align = "center", maxWidth = 125, #headerVAlign = "bottom", html = TRUE,
                                style = list(background = "rgba(0, 0, 0, 0.03)"),
          cell = reactablefmtr::icon_trend_indicator(.,
                                                     icons = "angle-double",
                                                     colors = c("darkgreen","grey","red"),
                                                     number_fmt = change_format)
        )
      )
    )
  })
}
