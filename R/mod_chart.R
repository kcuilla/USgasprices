#' mod_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_chart_ui <- function(id){
  ns <- NS(id)

  fullPage::fullContainer(
    center = TRUE,
    echarts4r::echarts4rOutput(ns("streamgraph"), height = "75vh")
  )
}

#' mod_table Server Functions
#'
#' @noRd

mod_chart_server <- function(input, output, session){
  ns <- session$ns
  
  city_cols <- c("#204f5e",
                 "#118ab2",
                 "#58adc9",
                 "#ffd166",
                 "#ffae1a",
                 "#ef476f",
                 "#51e2bd",
                 "#63848e",
                 "#f47e9a",
                 "#f9b5c5")

  output$streamgraph <- echarts4r::renderEcharts4r({
    gasprices::historical_data |>
      dplyr::filter(type == "Regular" & date >= "2007-01-01") |>
      dplyr::select(-`U.S.`) |>
      echarts4r::e_charts(date) |>
      echarts4r::e_river(`Boston, MA`, label = '{"show":"FALSE"}') |>
      echarts4r::e_river(`Chicago, IL`, label = '{"show":"FALSE"}') |>
      echarts4r::e_river(`Cleveland, OH`, label = '{"show":"FALSE"}') |>
      echarts4r::e_river(`Denver, CO`, label = '{"show":"FALSE"}') |>
      echarts4r::e_river(`Houston, TX`, label = '{"show":"FALSE"}') |>
      echarts4r::e_river(`Los Angeles, CA`, label = '{"show":"FALSE"}') |>
      echarts4r::e_river(`Miami, FL`, label = '{"show":"FALSE"}') |>
      echarts4r::e_river(`New York, NY`, label = '{"show":"FALSE"}') |>
      echarts4r::e_river(`San Francisco, CA`, label = '{"show":"FALSE"}') |>
      echarts4r::e_river(`Seattle, WA`, label = '{"show":"FALSE"}') |>
      echarts4r::e_tooltip(trigger = "axis",
                           renderMode = "richText") |>
      echarts4r::e_legend(textStyle = list(fontSize = "10")) |>
      echarts4r::e_color(city_cols)
  })
}


    