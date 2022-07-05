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

years <- gasprices::historical_data %>%
  dplyr::mutate(year = strftime(date, format = "%Y")) %>% 
  dplyr::arrange(year) %>% 
  dplyr::pull(year) %>% 
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
          br(),br(),br(),    
          shinyWidgets::radioGroupButtons(
            inputId = ns("selectgas"),
            label = h5("Gas Type:"),
            choices = c("Regular", "Midgrade", "Premium"),
            individual = TRUE,
            checkIcon = list(
              yes = icon("gas-pump")
            )
          ),
          column(width = 3,
                 offset = 3,
          shinyWidgets::pickerInput(
            inputId = ns("selectlocation"),
            label = "Location: ",
            choices = UScities,
            selected = "Boston, MA",
            choicesOpt = list(
              content = sprintf("<span class='dropdown'>%s</span>", UScities)
            ),
            options = list(
              style = "btn-default",
              size = 11,
              title = "Select City: "
            ),
            multiple = FALSE
          )),
          column(width = 3,
          shinyWidgets::sliderTextInput(
             inputId = ns("selectyear"),
             label = "Start Year:",
             grid = TRUE,
             force_edges = TRUE,
             choices = years
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
      echarts4r::e_tooltip(trigger = "axis",
                           renderMode = "richText") |>
      echarts4r::e_legend(textStyle = list(fontSize = "10")) |>
      echarts4r::e_color(city_cols)
  })
  
      
  output$waterfall <- highcharter::renderHighchart({
        
    filtereddf <- gasprices::historical_data %>%
        dplyr::filter(type == input$selectgas) %>%
        dplyr::select(1,location = input$selectlocation) %>%
        dplyr::mutate(week = strftime(date, format = "%V"),
                      year = strftime(date, format = "%Y")) %>%
        dplyr::filter(week == week[which.max(date)]) %>%
        dplyr::filter(year >= input$selectyear) %>% 
        dplyr::mutate(change = location - dplyr::lead(location)) 
      
      waterfalldf <- filtereddf %>% 
        dplyr::mutate(change = replace(change, nrow(filtereddf), location[nrow(filtereddf)])) %>%
        dplyr::arrange(date) 

      waterfalldf <- rbind(waterfalldf, transform(waterfalldf[rep(nrow(waterfalldf), 1),])) 
      
      waterfalldf <- waterfalldf %>%
        dplyr::mutate(a = c(rep(FALSE,nrow(waterfalldf)-1),TRUE)) %>% 
        dplyr::mutate(change = replace(change, nrow(waterfalldf), 0),
                      year = replace(year, nrow(waterfalldf), "This Week")) %>% 
        dplyr::mutate(color = dplyr::case_when(
          date == min(date) ~ "#999",
          year == "This Week" ~ "#999",
          change >= 0 ~ "#ff0000",
          change < 0 ~ "#006400",
          TRUE ~ "#777"
        )) %>% 
        dplyr::rename(values = 2) %>% 
        dplyr::mutate(change = replace(change, nrow(waterfalldf), values[nrow(waterfalldf)]))

      last_index <- nrow(waterfalldf)-1
      
      custom_tooltip <- highcharter::JS(paste0("function (){
        if(this.point.index == 0){
            return this.point.year + '<br>' + '<b>$' + Highcharts.numberFormat(this.point.values, 2) + ' per gal</b>';
        }else if(this.point.index == ",last_index,"){
            return this.point.year + '<br>' + '<b>$' + Highcharts.numberFormat(this.point.values, 2) + ' per gal</b>';
        }else if(this.point.change > 0){
            return this.point.year + '<br>' + '<b>+$' + Highcharts.numberFormat(this.point.change, 2) + '</b>';
        }else{
            return this.point.year + '<br>' + '<b>-$' + Highcharts.numberFormat(Math.abs(this.point.change, 2)) + '</b>';
        }
      }"))
      
    highcharter::highchart() %>%
      highcharter::hc_xAxis(type = "category") %>%
      highcharter::hc_add_series(data = waterfalldf, 
                                 type = "waterfall",
                                 borderColor = "transparent",
                                 style = list(fontFamily = "Arsenal"),
                                 highcharter::hcaes(x = year, y= change, isSum = a,
                                                    color = color
                                                    )) %>% 
      highcharter::hc_yAxis(max = 7, title = list(text = "Dollars per Gallon"), labels = list(format = "${value:.0f}")) %>% 
      highcharter::hc_legend(enabled = FALSE) %>% 
      highcharter::hc_tooltip(borderWidth = 3, formatter = custom_tooltip) %>% 
      highcharter::hc_title(
        text = glue::glue("How {input$selectgas} Gas Prices Have Changed in {input$selectlocation} Since {input$selectyear}"),
        style = list(fontFamily = "Arsenal",
                     fontSize = "16px")
      )
      })
}