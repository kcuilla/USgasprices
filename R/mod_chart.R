#' mod_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

# javascript animate numbers function
js <- "
  Shiny.addCustomMessageHandler('anim',
   function(x){

      var $box = $('#' + x.id + ' div.small-box');
      var value = x.value;
      var type = x.type;

      var $s = $box.find('div.inner h3');
      var o = {value: 0};

      $.Animation( o, {
          value: value
        }, {
          duration: 1500
        }).progress(function(e) {
            $s.text(type + '$' + (e.tweens[0].now).toFixed(2));
      });

    }
  );"

cities <- c("Boston, MA","Chicago, IL","Cleveland, OH","Denver, CO","Houston, TX","Los Angeles, CA","Miami, FL","New York, NY","San Francisco, CA","Seattle, WA")

mod_chart_ui <- function(id){
  ns <- NS(id)

  fluidRow(
  fullPage::pageContainer(
    fullPage::fullContainer(
      tags$head(tags$script(shiny::HTML(js))),
      br(),br(),br(),
      fluidRow(style = "margin-left: 26%; border: 1px inset #999999; width: 50%; align-items: center; justify-content: center;",
        column(
          offset = 2,
          width = 3,
          tagAppendAttributes(
            shinydashboard::valueBox("",
              subtitle = ""
            ),
            id = "regbox"
          )
        ),
        column(
          3,
          tagAppendAttributes(
            shinydashboard::valueBox("",
              subtitle = ""
            ),
            id = "midbox"
          )
        ),
        column(
          3,
          tagAppendAttributes(
            shinydashboard::valueBox("",
              subtitle = ""
            ),
            id = "prebox"
          )
        )),
        fluidRow(style = "align-items: center; justify-content: center;",
        column(
          uiOutput(ns("desc")),
          offset = 4,
          width = 4
          )
        ),
      br(),br(),
      fullPage::fullContainer(
        column(width = 5,
        shinyWidgets::pickerInput(
          inputId = ns("location"),
          label = NULL,
          width = "70%",
          choices = cities,
          selected = "Boston, MA",
          choicesOpt = list(
            content = sprintf("<span class='dropdown'>%s</span>", cities)
          ),
          options = list(
            style = "btn-default btn-lg",
            size = 10,
            title = "Select City: "
          ),
          multiple = FALSE
        )),
        column(width = 6,
               offset = 1,
        hover::use_hover(),
        hover::hover_action_button(
          inputId = ns("add"), label = "  Add U.S. Avg.", width = "35%", class = "dropdown",
          icon = icon("fas fa-plus-circle", style = "color: rgba(0,100,0,0.8)"),
          button_animation = "shadow grow"
        ),
        hover::hover_action_button(
          inputId = ns("rm"), label = "  Remove U.S. Avg.", width = "35%", class = "dropdown",
          icon = icon("fas fa-minus-circle", style = "color: rgba(255,0,0,0.8)"),
          button_animation = "shadow grow"
      ))),
      br(),
      fullPage::fullContainer(
        echarts4r::echarts4rOutput(ns("linechart"), height = "60vh")
      ),
      h5("Adjust slider to view particular timeframe")
    )
  )
)
}

#' mod_table Server Functions
#'
#' @noRd

mod_chart_server <- function(input, output, session){
  ns <- session$ns

  output$desc <- renderUI({
    msg <- paste0("As of: ", format(max(gasprices::summary_table$updated), "%b %d, %Y"))
    h4(msg)
  })

  reg <- reactive({
    gasprices::summary_table |>
      dplyr::filter(location == input$location & type == "Regular") |>
      dplyr::pull(value)
  })

  mid <- reactive({
    gasprices::summary_table |>
      dplyr::filter(location == input$location & type == "Midgrade") |>
      dplyr::pull(value)
  })

  pre <- reactive({
    gasprices::summary_table |>
      dplyr::filter(location == input$location & type == "Premium") |>
      dplyr::pull(value)
  })

  observeEvent(reg(), {
    session$sendCustomMessage("anim", list(id = "regbox", type = "Regular: ", value = reg()))
  })

  observeEvent(mid(), {
    session$sendCustomMessage("anim", list(id = "midbox", type = "Midgrade: ", value = mid()))
  })

  observeEvent(pre(), {
    session$sendCustomMessage("anim", list(id = "prebox", type = "Premium: ", value = pre()))
  })

  output$linechart <- echarts4r::renderEcharts4r({

    my_colors <- c("#f27405","#7c7c7c")

    df_final <- gasprices::historical_data |>
      dplyr::select(date, type, input$location) |>
      tidyr::pivot_wider(
        names_from = type,
        names_glue = "{type}_{.value}",
        values_from = input$location
        ) |>
      dplyr::rename(lwr = paste0("Regular_", input$location),
                    mid = paste0("Midgrade_", input$location),
                    upr = paste0("Premium_", input$location)) |>
      dplyr::mutate(avg = rowMeans(cbind(lwr, mid, upr), na.rm = TRUE))

      echarts4r::e_charts(df_final, date) |>
      echarts4r::e_band2(lower = lwr, upper = upr, name = input$location, tooltip = list(show = FALSE)) |>
      echarts4r::e_line(mid, name = input$location, lineStyle = list(width = "0"), symbolSize = "0") |>
      echarts4r::e_tooltip(trigger = "axis",
                           renderMode = "richText") |>
      echarts4r::e_axis_labels(y = "Dollars Per Gallon") |>
      echarts4r::e_legend(textStyle = list(fontSize = "16")) |>
      echarts4r::e_color(my_colors) |>
      echarts4r::e_datazoom(start = 73)
  })

  observeEvent(input$add, {

    us_avg <- gasprices::historical_data |>
      dplyr::group_by(date) |>
      dplyr::summarize(avg = round(mean(`U.S.`, na.rm = TRUE), digits = 2))

    echarts4r::echarts4rProxy(ns("linechart"), data = us_avg, x = date) |>  # create a proxy
      echarts4r::e_line(`avg`, lineStyle = list(width = "6",
                                                shadowColor = "rgba(0, 0, 0, 0.4)",
                                                shadowBlur = "8"),
                        symbolSize = "0", name = "U.S. Avg") |>
      echarts4r::e_execute()
  })

  observeEvent(input$rm, {
    echarts4r::echarts4rProxy(ns("linechart")) |> # create a proxy
      echarts4r::e_remove_serie("U.S. Avg")
  })

}

