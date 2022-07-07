#' mod_chart UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

UScities <- c("Boston, MA","Chicago, IL","Cleveland, OH","Denver, CO","Houston, TX","Los Angeles, CA","Miami, FL","New York, NY","San Francisco, CA","Seattle, WA","U.S.")
gastypes <- c("Regular","Midgrade","Premium")

years <- gasprices::historical_data |>
  dplyr::mutate(year = strftime(date, format = "%Y")) |> 
  dplyr::arrange(year) |> 
  dplyr::pull(year) |> 
  unique()

years <- years[-length(years)]
     
mod_chart_ui <- function(id){
  ns <- NS(id)

  fullPage::fullSlide(
  fullPage::fullContainer(
    center = TRUE,
    br(),
    echarts4r::echarts4rOutput(ns("streamgraph"), height = "75vh"),
        fullPage::fullSlide(
          fullPage::fullContainer(
          fullPage::fullRow(
          br(),br(),
          shinyWidgets::radioGroupButtons(
            inputId = ns("selectgas"),
            label = h5("Gas Type:"),
            choices = c("Regular", "Midgrade", "Premium"),
            individual = TRUE,
            checkIcon = list(
              yes = icon("gas-pump")
            )
          ),
          column(width = 4,
                 offset = 2,
          shinyWidgets::pickerInput(
            inputId = ns("selectlocation"),
            label = "Location:",
            choices = UScities,
            selected = "Boston, MA",
            width = "fit",
            choicesOpt = list(
              content = sprintf("<span class='uscity-dropdown'>%s</span>", UScities)
            ),
            options = list(
              style = "btn-default",
              size = length(UScities)
            ),
            multiple = FALSE
          )),
          column(width = 3,
          shinyWidgets::pickerInput(
            inputId = ns("selectyear"),
            label = "Start Year:",
            choices = years,
            selected = "2007",
            width = "fit",
            choicesOpt = list(
              content = sprintf("<span class='year-dropdown'>%s</span>", years)
            ),
            options = list(
              style = "btn-default",
              size = length(years)
            ),
            multiple = FALSE
          ))),
            highcharter::highchartOutput(ns("waterfall"), height = "55vh")
          ))
    )
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
      echarts4r::e_tooltip(trigger = "axis", renderMode = "richText") |>
      echarts4r::e_legend(textStyle = list(fontSize = "10")) |>
      echarts4r::e_color(city_cols)
  })
  
  output$waterfall <- highcharter::renderHighchart({
        
    filtereddf <- gasprices::historical_data |>
        dplyr::filter(type == input$selectgas) |>
        dplyr::select(1, location = input$selectlocation) |>
        dplyr::mutate(week = strftime(date, format = "%V"),
                      month = strftime(date, format = "%m"),
                      year = strftime(date, format = "%Y")) |>
        dplyr::filter(week == week[which.max(date)]) |>
        dplyr::filter(year >= input$selectyear) |> 
        dplyr::mutate(change = location - dplyr::lead(location)) 
      
      waterfalldf <- filtereddf |> 
        dplyr::mutate(change = replace(change, nrow(filtereddf), location[nrow(filtereddf)])) |>
        dplyr::arrange(date) 

      waterfalldf <- rbind(waterfalldf, transform(waterfalldf[rep(nrow(waterfalldf), 1),])) 
      
      waterfalldf <- waterfalldf |>
        dplyr::mutate(sum_indicator = c(rep(FALSE,nrow(waterfalldf)-1),TRUE)) |> 
        dplyr::mutate(change = replace(change, nrow(waterfalldf), 0),
                      year = replace(year, nrow(waterfalldf), "This Week")) |> 
        dplyr::mutate(color = dplyr::case_when(
          date == min(date) ~ "#666",
          year == "This Week" ~ "#7405f2",
          change >= 0 ~ "#ff0000",
          change < 0 ~ "#006400",
          TRUE ~ "#777"
        )) |> 
        dplyr::rename(values = 2) |> 
        dplyr::mutate(change = replace(change, nrow(waterfalldf), values[nrow(waterfalldf)]))
      
      gasprice_diff <- waterfalldf |>
        dplyr::summarize(difference = dplyr::last(values) - dplyr::first(values),
                         text_difference = format(dplyr::last(values) - dplyr::first(values), nsmall = 2)) |>
        dplyr::mutate(text_indicator = dplyr::case_when(
          difference >= 0 ~ "more",
          TRUE ~ "less"
        )) |>
        dplyr::mutate(color_indicator = dplyr::case_when(
          difference >= 0 ~ "#ff0000",
          TRUE ~ "#006400"
        ))

      start_year <- min(waterfalldf$year)
      
      custom_tooltip <- highcharter::JS(paste0("function (){
        if(this.point.year == ",start_year,"){
            return '<b>' + this.point.year + '</b><br>' + '$' + Highcharts.numberFormat(this.point.values, 2) + ' per gal';
        }else if(this.point.year == 'This Week'){
            return '<b>' + this.point.year + '</b><br>' + '$' + Highcharts.numberFormat(this.point.values, 2) + ' per gal';
        }else if(this.point.change > 0){
            return '<b>' + this.point.year + '</b><br>' + '+$' + Highcharts.numberFormat(this.point.change, 2);
        }else{
            return '<b>' + this.point.year + '</b><br>' + '-$' + Highcharts.numberFormat(Math.abs(this.point.change, 2));
        }
      }"))
      
      waterfalldf_neg <- waterfalldf |> dplyr::filter(change < 0)
      waterfalldf_pos <- waterfalldf |> dplyr::filter(change > 0)
      
      highcharter::highchart() |>
        highcharter::hc_xAxis(type = "category") |>
        highcharter::hc_yAxis(
          max = 7,
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
          highcharter::hcaes(
            x = year,
            y = values, 
            color = color
          )
        ) |>
        highcharter::hc_add_series(
          data = waterfalldf_pos,
          type = "line",
          lineColor = "transparent",
          animation = list(defer = 1),
          marker = list(symbol = "triangle", radius = 8),
          highcharter::hcaes(
            x = year,
            y = values, 
            color = color
          )
        ) |>
        highcharter::hc_plotOptions(
          series = list(
            states = list(
              inactive = list(opacity = 1)
            ))
        ) |> 
        highcharter::hc_legend(enabled = FALSE) |>
        highcharter::hc_tooltip(
          borderWidth = 3,
          formatter = custom_tooltip,
          crosshairs = TRUE,
          style = list(fontFamily = "Arsenal", fontSize = "16px")
        ) |>
        highcharter::hc_title(
          text = glue::glue(
            "In <span style='color:#f27405; font-weight:bold;'>{input$selectlocation}</span>, 
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
