#' mod_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

cities <-
  c(
    "Boston, MA",
    "Chicago, IL",
    "Cleveland, OH",
    "Denver, CO",
    "Houston, TX",
    "Los Angeles, CA",
    "Miami, FL",
    "NYC, NY",
    "San Francisco, CA",
    "Seattle, WA"
  )

mod_table_ui <- function(id) {
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
          checkIcon = list(yes = icon("gas-pump"))
        )
      ),
      fluidRow(
        column(
          width = 2,
          offset = 7,
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
          )
        )
      ),
      reactable::reactableOutput(ns("table"), height = "60vh"),
      uiOutput(ns("desc"), class = "table-as-of-date"),
      shinybrowser::detect()
    ),
    fullPage::fullSlide(
      fullPage::fullContainer(
        br(),br(),br(),br(),
        fullPage::fullContainer(br(),br(),br(),br(),
                                ggiraph::ggiraphOutput(ns("map"))),
        br(),br(),
        uiOutput(ns("desc2"), class = "chart-as-of-date")
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
    msg <- paste0("As of: ", format(max(summary_table$updated), "%m/%d/%y"))
    h5(msg)
  })

  max_price <- max(summary_table$value)
  
  ### get width from browser
  device_reactive <- reactive({
    if (as.numeric(shinybrowser::get_width()) <= 480) {
      "mobile"
    } else {
      "computer"
    }
  })
  
  output$table <- reactable::renderReactable({
    build_table <- function(type) {
      if (type == "mobile") {
        showhide <- FALSE
        location_width <- 100
        gal_width <- 110
        change_width <- 70
        font_size <- 14
        header_font_size <- 12
        sanfran <- "SF, CA"
        losangeles <- "LA, CA"
      } else {
        showhide <- TRUE
        location_width <- 130
        gal_width <- 175
        change_width <- 80
        font_size <- 14
        header_font_size <- 12
        sanfran <- "San Francisco, CA"
        losangeles <- "Los Angeles, CA"
      }
      
      change_format <- switch(
        input$change,
        "dollar" = scales::dollar_format(accuracy = 0.01),
        "percent" = scales::percent_format(accuracy = 0.1)
      )
      
      summary_table %>%
        dplyr::filter(type == input$type) %>%
        dplyr::select(1:7, dplyr::starts_with(input$change)) %>%
        dplyr::rename(wow = 8, mom = 9, yoy = 10) %>%
        dplyr::mutate(fill_colors = dplyr::case_when(location == "U.S." ~ "#7c7c7c",
                                                     TRUE ~ "#f27405")) %>%
        dplyr::mutate(
          location = dplyr::case_when(
            location == "San Francisco, CA" ~ sanfran,
            location == "Los Angeles, CA" ~ losangeles,
            TRUE ~ location
          )
        ) %>%
        reactable::reactable(
          theme = reactablefmtr::no_lines(
            background_color = "#FFFFFF",
            font_size = font_size,
            header_font_size = header_font_size,
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
            location = reactable::colDef(name = "LOCATION", width = location_width),
            max_value = reactable::colDef(show = FALSE),
            value = reactable::colDef(
              name = "$/GAL",
              align = "center",
              width = gal_width,
              cell = reactablefmtr::data_bars(
                .,
                background = "transparent",
                text_position = "center",
                animation = "width 0.4s linear",
                bar_height = 26,
                max_value = max_price,
                fill_color_ref = "fill_colors",
                fill_opacity = 0.9,
                number_fmt = scales::dollar_format(accuracy = 0.01)
              )
            ),
            wow = reactable::colDef(
              name = "VS <br> LAST WK",
              align = "center",
              maxWidth = change_width,
              style = list(background = "rgba(0, 0, 0, 0.03)"),
              cell = reactablefmtr::icon_trend_indicator(
                .,
                icons = "angle-double",
                colors = c("darkgreen", "grey", "red"),
                number_fmt = change_format
              )
            ),
            mom = reactable::colDef(
              name = "VS <br> LAST MO",
              align = "center",
              maxWidth = change_width,
              show = showhide,
              style = list(background = "rgba(0, 0, 0, 0.03)"),
              cell = reactablefmtr::icon_trend_indicator(
                .,
                icons = "angle-double",
                colors = c("darkgreen", "grey", "red"),
                number_fmt = change_format
              )
            ),
            yoy = reactable::colDef(
              name = "VS <br> LAST YR",
              align = "center",
              maxWidth = change_width,
              style = list(background = "rgba(0, 0, 0, 0.03)"),
              cell = reactablefmtr::icon_trend_indicator(
                .,
                icons = "angle-double",
                colors = c("darkgreen", "grey", "red"),
                number_fmt = change_format
              )
            )
          )
        )
    }
    build_table(type = device_reactive())
  })
  
  
  output$desc2 <- renderUI({
    msg <- paste0("As of: ", format(max(summary_table$updated), "%m/%d/%y"))
    msg
  })


  output$linechart <- highcharter::renderHighchart({
    lc_data <- historical_data |>
      dplyr::filter(date >= (max(date) - 734)) |>
      dplyr::select(date, type, input$location) |>
      tidyr::pivot_wider(
        names_from = type,
        names_glue = "{type}_{.value}",
        values_from = input$location
      ) |>
      dplyr::rename(
        lwr = paste0("Regular_", input$location),
        mid = paste0("Midgrade_", input$location),
        upr = paste0("Premium_", input$location)
      ) |>
      dplyr::arrange(date) |>
      dplyr::mutate(avg = rowMeans(cbind(lwr, mid, upr), na.rm = TRUE),
                    date = format(as.Date(date), "%m-%d-%Y"))
    
    lc_data_highchart <-
      highcharter::list_parse2(lc_data |> dplyr::select(-c(mid, avg)))
    
    chart_tooltip <- highcharter::JS(
      paste0(
        "function (){
    if(this.series.name != 'Range'){
      return '<b>' + this.series.name + '</b><br>' +
        '$' + Highcharts.numberFormat(this.point.y, 2) + ' per gal';
    }else{
      return '<b>' + this.series.name + '</b><br>' +
        'Regular $' + Highcharts.numberFormat(this.point.y[1], 2) + ' per gal' + '<br>' +
        'Premium $' + Highcharts.numberFormat(this.point.y[2], 2) + ' per gal';
    }"
      )
    )
    
    highcharter::highchart() |>
      highcharter::hc_xAxis(type = "datetime") |>
      highcharter::hc_yAxis(
        min = 0,
        max = 7,
        tickInterval = 1,
        title = list(text = "$/gal"),
        labels = list(format = "${value:.0f}")
      ) |>
      highcharter::hc_add_series(
        data = lc_data_highchart,
        name = "Range",
        type = "arearange",
        color = "#f27405",
        opacity = 0.5
      ) |>
      highcharter::hc_add_series(
        data = lc_data,
        name = "Avg.",
        type = "line",
        lineWidth = 4,
        color = "#f27405",
        highcharter::hcaes(x = date,
                           y = avg)
      ) |>
      highcharter::hc_tooltip(
        borderWidth = 3,
        formatter = chart_tooltip,
        style = list(fontFamily = "Arsenal", fontSize = "16px")
      ) |>
      highcharter::hc_title(
        text = glue::glue(
          "<span style='color:#f27405; font-weight:bold;'>{input$location}</span>"
        ),
        useHTML = TRUE,
        style = list(
          fontFamily = "Arsenal",
          fontSize = "17px",
          color = "#777"
        )
      )
  })
  
  
  city_locations <- data.frame(
    city = c("Boston","Chicago","Cleveland","Denver","Houston","Los Angeles","Miami","New York","San Francisco","Seattle","U.S."),
    lat = c(42.4,41.9,41.6,39.8,29.8,34.0,25.8,40.7,37.7,47.6,39.1),
    lon = c(-71.1,-87.6,-81.5,-105,-95.4,-118,-80.3,-73.9,-122,-122,-95.7)
  )
  
  city_data <- summary_table |> 
    dplyr::mutate(city = gsub("(.*),.*", "\\1", location)) |> 
    dplyr::inner_join(city_locations) |> 
    dplyr::mutate(map_label = dplyr::case_when(
    city == "San Francisco" ~ "San <br> Francisco",
    city == "Los Angeles" ~ "Los <br> Angeles",
    city == "New York" ~ "NYC",
    TRUE ~ city
    )) |> 
    dplyr::filter(type == "Regular")

  cities_t <- usmap::usmap_transform(city_data)

  make_tooltip_table <- function(city) {
    df <- historical_data |>
      dplyr::mutate(year = strftime(date, format = "%Y")) |>
      dplyr::filter(year == max(year)) |>
      dplyr::arrange(date, city) |>
      dplyr::select(1:2, value = city) |>
      dplyr::mutate(location = city)
    
    limits <- range(df$value)
    
    tooltip_table <- df |>
      dplyr::group_by(Type = type, location) |>
      dplyr::summarize(
        `$/gal` = tail(value, 1),
        data = list(value),
        .groups = "drop"
      ) |>
      dplyr::mutate(color_assign = ifelse(location == "U.S.", "#7c7c7c", "#f27405")) |>
      dplyr::mutate(plot = purrr::map(
        data,
        ~ kableExtra::spec_plot(
          .x,
          ylim = limits,
          height = 75,
          width = 225,
          same_lim = TRUE,
          polymin = limits[[1]],
          col = color_assign,
          min = list(pch = ".", cex = 1, col = "darkgreen"),
          max = list(pch = ".", cex = 2, col = "red")
        )
      )) |>
      dplyr::select(-c(location, data, color_assign)) |>
      dplyr::arrange(`$/gal`)
    
    tooltip_table |>
      dplyr::select(-plot) |>
      dplyr::mutate(`$/gal` = scales::dollar(`$/gal`)) |>
      dplyr::mutate(`YTD Trend` = "") |>
      kableExtra::kbl(
        caption = paste0("<center><strong>", city, "</strong></center>"),
        escape = FALSE,
        format = "html"
      ) |>
      kableExtra::column_spec(1, width = "5em") |>
      kableExtra::column_spec(2, width = "3em") |>
      kableExtra::column_spec(3, image = tooltip_table$plot) |>
      kableExtra::kable_styling(htmltable_class = "lighttable-minimal", html_font = "Arsenal")
  }
    
    cities_t <- cities_t |>
      dplyr::mutate(plots = purrr::map(location, make_tooltip_table))


    output$map <- ggiraph::renderGirafe({
      gas_map <-
        usmap::plot_usmap(
          fill = "lightgrey",
          color = "white",
          size = 0.35,
          alpha = 0.2
        ) +
        ggtext::geom_richtext(
          data = cities_t,
          fill = NA,
          label.color = NA,
          ggplot2::aes(x = x, y = y, label = map_label),
          family = "Roboto"
        ) +
        ggiraph::geom_point_interactive(
          data = cities_t,
          ggplot2::aes(
            x = x,
            y = y,
            size = value,
            tooltip = plots
          ),
          color = ifelse(cities_t$location == "U.S.", "#7c7c7c", "#f27405"),
          alpha = 0.5
        ) +
        ggplot2::scale_size_continuous(range = c(10, 25),
                                       label = scales::comma) +
        ggplot2::ggtitle("Current Gas Prices in U.S. Cities") +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            color = "#333333",
            size = 14,
            hjust = 0.5,
            face = "bold"
          ),
          legend.position = "none"
        )
      
      ggiraph::girafe(ggobj = gas_map)
      
    })
}
