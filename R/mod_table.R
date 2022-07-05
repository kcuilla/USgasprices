#' mod_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

# custom javascript function to animate numbers
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

mod_table_ui <- function(id){
  ns <- NS(id)
  
  fullPage::fullSlide(
    fullPage::fullContainer(
      fluidRow(
        br(),br(),br(),
        shinyWidgets::radioGroupButtons(
          inputId = ns("type"),
          label = h5("Gas Type:"),
          choices = c("Regular", "Midgrade", "Premium"),
          individual = TRUE,
          checkIcon = list(
            yes = icon("gas-pump")
          )
        )
      ),
      fluidRow(
        column(
          width = 2, offset = 7,
        shinyWidgets::prettyRadioButtons(
          inputId = ns("change"),
          label = NULL,
          choiceNames = c("$ Change", "% Change"),
          choiceValues = c("dollar", "percent"),
          inline = TRUE,
          fill = TRUE,
          thick = TRUE,
          status = "success",
          animation = "pulse"
        ))
      ),
      reactable::reactableOutput(ns("table"), height="60vh"),
      uiOutput(ns("desc"), class = "table-as-of-date")
      ),
      fullPage::fullSlide(
        fullPage::fullContainer(
            tags$head(tags$script(shiny::HTML(js))),
            br(),br(),br(),
            fluidRow(style = "border: 1px solid #999999; display: flex; max-width: 100%; align-items: center; justify-content: center; margin-left: auto; margin-right: auto;",
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
            uiOutput(ns("desc2"), class = "chart-as-of-date"),
            br(),
            fullPage::fullRow(
              column(width = 2,
                     offset = 1,
              shinyWidgets::pickerInput(
                inputId = ns("location"),
                label = NULL,
                choices = cities,
                selected = "Boston, MA",
                choicesOpt = list(
                  content = sprintf("<span class='dropdown'>%s</span>", cities)
                ),
                options = list(
                  style = "btn-default",
                  size = 10,
                  title = "Select City: "
                ),
                multiple = FALSE
              )),
              column(width = 5,
                     offset = 3,
              hover::use_hover(),
              hover::hover_action_button(
                inputId = ns("add"), label = "  U.S. Avg.", width = "35%", class = "dropdown",
                icon = icon("fas fa-plus-circle", style = "color: rgba(0,100,0,0.8)"),
                button_animation = "shadow grow"
              ),
              hover::hover_action_button(
                inputId = ns("rm"), label = "  U.S. Avg.", width = "35%", class = "dropdown",
                icon = icon("fas fa-minus-circle", style = "color: rgba(255,0,0,0.8)"),
                button_animation = "shadow grow"
            ))),
            br(),
            fullPage::fullContainer(
              echarts4r::echarts4rOutput(ns("linechart"), height = "60vh")
              )
            )
          )
        )
}

#' mod_table Server Functions
#'
#' @noRd

mod_table_server <- function(input, output, session){
  ns <- session$ns

  output$desc <- renderUI({
    msg <- paste0("As of: ", format(max(gasprices::summary_table$updated), "%m/%d/%y"))
    h5(msg)
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
      dplyr::mutate(location = dplyr::case_when(
        location == "San Francisco, CA" ~ "SF, CA",
        location == "Los Angeles, CA" ~ "LA, CA",
        TRUE ~ location
      )) %>% 
    reactable::reactable(
      theme = reactablefmtr::no_lines(
        background_color = "#FFFFFF",
        font_size = 14,
        header_font_size = 12,
        cell_padding = 2,
        centered = TRUE
      ),
      defaultSortOrder = "desc",
      defaultSorted = "value",
      pagination = FALSE,
      fullWidth = FALSE,
      style = list(fontFamily = "Roboto, sans-serif"),
      defaultColDef = reactable::colDef(headerVAlign = "bottom", html = TRUE),
      columns = list(
        type = reactable::colDef(show = FALSE),
        updated = reactable::colDef(show = FALSE),
        max_date = reactable::colDef(show = FALSE),
        date = reactable::colDef(show = FALSE),
        fill_colors = reactable::colDef(show = FALSE),
        location = reactable::colDef(name = "LOCATION"),
        max_value = reactable::colDef(show = FALSE),
        value = reactable::colDef(name = "PER GALLON", align = "center",
          cell = reactablefmtr::data_bars(.,
                                          background = "transparent",
                                          text_position = "center",
                                          animation = "width 0.4s linear",
                                          bar_height = 26,
                                          max_value = max_price,
                                          fill_color_ref = "fill_colors",
                                          number_fmt = scales::dollar_format(accuracy = 0.01))
        ),
        wow = reactable::colDef(name = "VS <br> LAST WK", align = "center", maxWidth = 66, 
                                style = list(background = "rgba(0, 0, 0, 0.03)"),
          cell = reactablefmtr::icon_trend_indicator(.,
                                                     icons = "angle-double",
                                                     colors = c("darkgreen","grey","red"),
                                                     number_fmt = change_format)
        ),
        mom = reactable::colDef(name = "VS <br> LAST MO", align = "center", maxWidth = 66,
                                style = list(background = "rgba(0, 0, 0, 0.03)"),
          cell = reactablefmtr::icon_trend_indicator(.,
                                                     icons = "angle-double",
                                                     colors = c("darkgreen","grey","red"),
                                                     number_fmt = change_format)
        ),
        yoy = reactable::colDef(name = "VS <br> LAST YR", align = "center", maxWidth = 66, 
                                style = list(background = "rgba(0, 0, 0, 0.03)"),
          cell = reactablefmtr::icon_trend_indicator(.,
                                                     icons = "angle-double",
                                                     colors = c("darkgreen","grey","red"),
                                                     number_fmt = change_format)
        )
      )
    )
  })
  
  output$desc2 <- renderUI({
    msg <- paste0("As of: ", format(max(gasprices::summary_table$updated), "%m/%d/%y"))
    msg
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
      dplyr::filter(date >= (max(date) - 734)) |>
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
      echarts4r::e_axis_labels(y = "$/gal") |>
      echarts4r::e_y_axis(max = 7) |>
      echarts4r::e_legend(textStyle = list(fontSize = "15")) |>
      echarts4r::e_color(my_colors) 
  })

  observeEvent(input$add, {

    us_avg <- gasprices::historical_data |>
      dplyr::filter(date >= (max(date) - 734)) |>
      dplyr::group_by(date) |>
      dplyr::summarize(avg = round(mean(`U.S.`, na.rm = TRUE), digits = 2))

    echarts4r::echarts4rProxy(ns("linechart"), data = us_avg, x = date) |>  
      echarts4r::e_line(`avg`, lineStyle = list(width = "4",
                                                shadowColor = "rgba(0, 0, 0, 0.4)",
                                                shadowBlur = "6"),
                        symbolSize = "0", name = "U.S. Avg.") |>
      echarts4r::e_execute()
  })

  observeEvent(input$rm, {
    echarts4r::echarts4rProxy(ns("linechart")) |> 
      echarts4r::e_remove_serie("U.S. Avg.")
  })
}
