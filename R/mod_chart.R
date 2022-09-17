#' mod_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_chart_ui <- function(id) {
  ns <- NS(id)
  
  UScities <- c("U.S.","Boston, MA","Chicago, IL","Cleveland, OH","Denver, CO","Houston, TX","Los Angeles, CA","Miami, FL","NYC, NY","San Francisco, CA","Seattle, WA")
gastypes <- c("Regular","Midgrade","Premium")

  years <- gasprices::historical_data |>
    dplyr::mutate(year = strftime(date, format = "%Y")) |> 
    dplyr::arrange(year) |> 
    dplyr::pull(year) |> 
    unique()
  
  years <- years[-length(years)]
    
  fullPage::fullSlide(
    fullPage::fullContainer(
      center = TRUE,
      fullPage::fullRow(
        br(),
        br(),
        br(),
        shinyWidgets::radioGroupButtons(
          inputId = ns("selectgasarea"),
          label = h5("Gas Type:"),
          choices = c("Regular", "Midgrade", "Premium"),
          individual = TRUE,
          checkIcon = list(yes = icon("gas-pump"))
        ),
        column(
          width = 2,
          offset = 1,
          shinyWidgets::pickerInput(
            inputId = ns("arealocation"),
            label = NULL,
            choices = cities,
            selected = "Boston, MA",
            choicesOpt = list(content = sprintf(
              "<span class='dropdown'>%s</span>", cities
            )),
            options = list(
              style = "btn-default",
              size = 10,
              title = "Select City: "
            ),
            multiple = FALSE
          )
        ),
        column(
          width = 5,
          offset = 3,
          hover::use_hover(),
          hover::hover_action_button(
            inputId = ns("add"),
            label = "  U.S. Avg.",
            width = "35%",
            class = "dropdown",
            icon = icon("fas fa-plus-circle", style = "color: rgba(0,100,0,0.8)"),
            button_animation = "shadow grow"
          )
        )
      ),
      highcharter::highchartOutput(ns("areachart"), height = "60vh")
    ),
    fullPage::fullSlide(
      fullPage::fullContainer(
        center = TRUE,
        fullPage::fullRow(
          br(),
          br(),
          shinyWidgets::radioGroupButtons(
            inputId = ns("selectgas"),
            label = h5("Gas Type:"),
            choices = c("Regular", "Midgrade", "Premium"),
            individual = TRUE,
            checkIcon = list(yes = icon("gas-pump"))
          ),
          column(
            width = 4,
            offset = 2,
            shinyWidgets::pickerInput(
              inputId = ns("selectlocation"),
              label = "Location:",
              choices = UScities,
              selected = "U.S.",
              width = "fit",
              choicesOpt = list(
                content = sprintf("<span class='uscity-dropdown'>%s</span>", UScities)
              ),
              options = list(style = "btn-default",
                             size = length(UScities)),
              multiple = FALSE
            )
          ),
          column(
            width = 3,
            shinyWidgets::pickerInput(
              inputId = ns("selectyear"),
              label = "Start Year:",
              choices = years,
              selected = "2007",
              width = "fit",
              choicesOpt = list(content = sprintf(
                "<span class='year-dropdown'>%s</span>", years
              )),
              options = list(style = "btn-default",
                             size = length(years)),
              multiple = FALSE
            )
          )
        ),
        highcharter::highchartOutput(ns("waterfall"), height = "60vh")
      )
    )
  )
}

#' mod_table Server Functions
#'
#' @noRd

mod_chart_server <- function(input, output, session) {
  ns <- session$ns
  
  observeEvent(input$add, {
    insertUI(
      "#linechart-add",
      "afterEnd",
      hover::hover_action_button(
        inputId = ns("rm"),
        label = "  U.S. Avg.",
        width = "35%",
        class = "dropdown",
        icon = icon("fas fa-minus-circle", style = "color: rgba(255,0,0,0.8)"),
        button_animation = "shadow grow"
      )
    )
    
    observeEvent(input$rm, {
      removeUI("#linechart-rm")
    }, ignoreInit = TRUE, once = TRUE)
  })
  
  area_tooltip <- highcharter::JS(
    paste0(
      "function (){
        return '<b>' + this.point.x + '</b><br>' +
        '$' + Highcharts.numberFormat(this.point.y, 2) + ' per gal';
    }"
    )
  )
  
  output$areachart <- highcharter::renderHighchart({
    gasprices::historical_data |>
      dplyr::filter(type == input$selectgasarea) |>
      dplyr::select(1, location = input$arealocation) |>
      highcharter::hchart(
        type = "area",
        highcharter::hcaes(x = date, y = location),
        color = "#f27405",
        lineWidth = 3,
        fillOpacity = 0.2,
        name = input$arealocation
      ) |>
      highcharter::hc_yAxis(
        min = 0,
        max = 7,
        tickInterval = 1,
        gridLineDashStyle = "shortdash",
        title = list(text = ""),
        labels = list(format = "${value:.0f}",
                      style = list(color = "#a1a1a1"))
      ) |>
      highcharter::hc_xAxis(
        tickColor = "#e6e6e6",
        lineColor = "#e6e6e6",
        title = list(text = ""),
        labels = list(style = list(color = "#a1a1a1")),
        plotLines = list(
          list(
            value = as.Date("2007-12-31") |> highcharter::datetime_to_timestamp(),
            color = "#333333",
            zIndex = 2,
            label = list(
              text = "The Great Recession",
              style = list(
                fontFamily = "Roboto",
                fontSize = "12px",
                color = "#333333"
              )
            )
          ),
          list(
            value = as.Date("2020-03-11") |> highcharter::datetime_to_timestamp(),
            color = "#333333",
            zIndex = 2,
            label = list(
              text = "COVID-19 Pandemic",
              style = list(
                fontFamily = "Roboto",
                fontSize = "12px",
                color = "#333333"
              )
            )
          ),
          list(
            value = as.Date("2022-02-24") |> highcharter::datetime_to_timestamp(),
            color = "#333333",
            zIndex = 2,
            label = list(
              text = "Russian Invasion of Ukraine",
              style = list(
                fontFamily = "Roboto",
                fontSize = "12px",
                color = "#333333"
              )
            )
          )
        )
      ) |>
      highcharter::hc_tooltip(
        borderWidth = 0,
        borderColor = NULL,
        crosshairs = TRUE,
        shared = TRUE,
        useHTML = TRUE,
        xDateFormat = '<b>%b %d, %Y</b>',
        headerFormat = '<table style="width: 100%">
                       <tr><th style="font-size:12px; padding-bottom:5px; border-bottom:1px solid lightgrey;">{point.key}</th>
                       <th style="font-size:12px; padding-bottom:5px; border-bottom:1px solid lightgrey;"></th></tr>',
        pointFormat = '<tr><td style=" padding-top:2px; color:{series.color};"><b>{series.name}:</b></td>
                      <td style="padding-top:2px; padding-left:5px;"><b>${point.y:.2f}</b></td></tr>',
        footerFormat = '</table>',
        style = list(fontFamily = "Arsenal", fontSize = "15px")
      ) |>
      highcharter::hc_title(
        align = "center",
        style = list(color = "#333333"),
        useHTML = TRUE,
        text = glue::glue(
          "Cost per gallon of <span style='text-transform:lowercase;'>{input$selectgasarea}</span> gas since 2007"
        )
      ) |>
      highcharter::hc_add_theme(highcharter::hc_theme_bloom())
  })
  
  observeEvent(input$add, {
    us_series <- gasprices::historical_data |>
      dplyr::filter(type == "Regular") |>
      dplyr::select(1, location = "U.S.")
    
    highcharter::highchartProxy(ns("areachart")) |>
      highcharter::hcpxy_add_series(
        data = us_series,
        id = "U.S. Avg.",
        type = "area",
        highcharter::hcaes(x = date, y = location),
        color = "#7c7c7c",
        lineWidth = 3,
        fillOpacity = 0.2,
        name = "U.S. Avg."
      )
  })
  
  observeEvent(input$rm, {
    highcharter::highchartProxy(ns("areachart")) |>
      highcharter::hcpxy_remove_series(id = "U.S. Avg.")
  })
  
  output$waterfall <- highcharter::renderHighchart({
    filtereddf <- gasprices::historical_data |>
      dplyr::filter(type == input$selectgas) |>
      dplyr::select(1, location = input$selectlocation) |>
      dplyr::mutate(
        week = strftime(date, format = "%V"),
        month = strftime(date, format = "%m"),
        year = strftime(date, format = "%Y")
      ) |>
      dplyr::filter(week == week[which.max(date)]) |>
      dplyr::filter(year >= input$selectyear) |>
      dplyr::mutate(change = location - dplyr::lead(location))
    
    waterfalldf <- filtereddf |>
      dplyr::mutate(change = replace(change, nrow(filtereddf), location[nrow(filtereddf)])) |>
      dplyr::arrange(date)
    
    waterfalldf <-
      rbind(waterfalldf, transform(waterfalldf[rep(nrow(waterfalldf), 1), ]))
    
    waterfalldf <- waterfalldf |>
      dplyr::mutate(sum_indicator = c(rep(FALSE, nrow(waterfalldf) - 1), TRUE)) |>
      dplyr::mutate(change = replace(change, nrow(waterfalldf), 0),
                    year = replace(year, nrow(waterfalldf), "This Week")) |>
      dplyr::mutate(
        color = dplyr::case_when(
          date == min(date) ~ "#666",
          year == "This Week" ~ "#7405f2",
          change >= 0 ~ "#ff0000",
          change < 0 ~ "#006400",
          TRUE ~ "#777"
        )
      ) |>
      dplyr::rename(values = 2) |>
      dplyr::mutate(change = replace(change, nrow(waterfalldf), values[nrow(waterfalldf)]))
    
    gasprice_diff <- waterfalldf |>
      dplyr::summarize(
        difference = dplyr::last(values) - dplyr::first(values),
        text_difference = format(dplyr::last(values) - dplyr::first(values), nsmall = 2)
      ) |>
      dplyr::mutate(text_indicator = dplyr::case_when(difference >= 0 ~ "more",
                                                      TRUE ~ "less")) |>
      dplyr::mutate(color_indicator = dplyr::case_when(difference >= 0 ~ "#ff0000",
                                                       TRUE ~ "#006400"))
    
    start_year <- min(waterfalldf$year)
    
    waterfalldf_neg <- waterfalldf |> dplyr::filter(change < 0)
    waterfalldf_pos <- waterfalldf |> dplyr::filter(change > 0)
    
    waterfall_tooltip <- highcharter::JS(
      paste0(
        "function (){
        if(this.point.year == ",
        start_year,
        "){
            return '<b>' + this.point.year + '</b><br>' + '$' + Highcharts.numberFormat(this.point.values, 2) + ' per gal';
        }else if(this.point.year == 'This Week'){
            return '<b>' + this.point.year + '</b><br>' + '$' + Highcharts.numberFormat(this.point.values, 2) + ' per gal';
        }else if(this.point.change > 0){
            return '<b>' + this.point.year + '</b><br>' + '+$' + Highcharts.numberFormat(this.point.change, 2);
        }else{
            return '<b>' + this.point.year + '</b><br>' + '-$' + Highcharts.numberFormat(Math.abs(this.point.change, 2));
        }
      }"
      )
    )
    
    theUS <- ifelse(input$selectlocation == "U.S.", "the ", "")
    
    highcharter::highchart() |>
      highcharter::hc_xAxis(type = "category") |>
      highcharter::hc_yAxis(
        min = 0,
        max = 7,
        tickInterval = 1,
        title = list(text = "$/gal"),
        labels = list(format = "${value:.0f}")
      ) |>
      highcharter::hc_add_series(
        data = waterfalldf,
        type = "waterfall",
        borderColor = "transparent",
        lineWidth = 1.5,
        pointWidth = 6,
        minPointLength = 3,
        style = list(fontFamily = "Arsenal", fontSize = "12px"),
        highcharter::hcaes(
          x = year,
          y = change,
          isSum = sum_indicator,
          color = color
        )
      ) |>
      highcharter::hc_add_series(
        data = waterfalldf_neg,
        type = "line",
        lineColor = "transparent",
        animation = list(defer = 1),
        marker = list(symbol = "triangle-down", radius = 8),
        highcharter::hcaes(x = year,
                           y = values,
                           color = color)
      ) |>
      highcharter::hc_add_series(
        data = waterfalldf_pos,
        type = "line",
        lineColor = "transparent",
        animation = list(defer = 1),
        marker = list(symbol = "triangle", radius = 8),
        highcharter::hcaes(x = year,
                           y = values,
                           color = color)
      ) |>
      highcharter::hc_plotOptions(series = list(states = list(inactive = list(opacity = 1)))) |>
      highcharter::hc_legend(enabled = FALSE) |>
      highcharter::hc_tooltip(
        borderWidth = 3,
        formatter = waterfall_tooltip,
        crosshairs = TRUE,
        style = list(fontFamily = "Arsenal", fontSize = "16px")
      ) |>
      highcharter::hc_title(
        text = glue::glue(
          "In ",
          theUS,
          "<span style='color:#f27405; font-weight:bold;'>{input$selectlocation}</span>,
            the cost of <span style='font-style:italic; text-transform:lowercase;'>{input$selectgas}</span>
            gas <span style='color:#7405f2; font-weight: bold;'>this week</span>
            is <span style='color:{gasprice_diff$color_indicator}; font-weight: bold;'>${gasprice_diff$text_difference} {gasprice_diff$text_indicator}</span>
            per gallon compared to <span style='color:#666; font-weight: bold;'>{input$selectyear}</span>"
        ),
        useHTML = TRUE,
        style = list(
          fontFamily = "Arsenal",
          fontSize = "17px",
          color = "#777"
        )
      )
  })
}
