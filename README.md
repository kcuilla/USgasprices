# U.S. Gas Prices Visualized

The [U.S. Gas Prices](https://kcuilla.shinyapps.io/usgasprices/) app displays the latest weekly gas prices from major cities across the U.S.. The data is pulled automatically from the U.S. Energy Information Administration's [weekly gas update](https://www.eia.gov/petroleum/gasdiesel/).

<img src="https://raw.githubusercontent.com/kcuilla/USgasprices/main/demo/streamgraph.PNG" />

## About the data

The data is sourced from the U.S. Energy Information Administration [(EIA)](https://www.eia.gov/). At the start of each week, the EIA updates the average prices for regular, midgrade, and premium retail gas. The data is available for a handful of cities across the U.S., as well as the overall average for the U.S..

This data is accessible via EIA's API. I used the {eia} package is used to connect to the API and pull the data in a tidy format. 

## Automatic updating

The data is updated every Tuesday evening after the EIA refreshes the data on their site. This is done automatically via GitHub Actions. Once the new data is pulled, the visualizations and text are refreshed and the updated app is automatically deployed to shinyapps.io. The script for the GitHub Actions workflow can be found [here](https://github.com/kcuilla/USgasprices/tree/main/.github/workflows).

## Shiny app

The framework for this Shiny app was made using the [{golem}](https://thinkr-open.github.io/golem/) package and displayed using the [{fullPage}](https://fullpage.rinterface.com/index.html) package by [John Coene](https://github.com/JohnCoene). 

A huge thank you to John for creating many packages that are used for this app, including the aforementioned {fullPage} package, [{echarts4r}](https://echarts4r.john-coene.com/index.html) and [{typedjs}](https://github.com/JohnCoene/typedjs). This app was also largely inspired by John's [Freedom of the Press](https://gallery.shinyapps.io/freedom-press-index/?_ga=2.217079061.223099009.1656718664-1490211595.1582849274) app which can be found in RStudio's [Shiny Gallery](https://shiny.rstudio.com/gallery/).

## Mobile & Desktop Friendly

The site is optimized for viewing on both mobile and desktop devices. I used the {shinybrowser} package to detect the user's device and display the visuals so that they optimally fit on the user's device. 

<img src="https://raw.githubusercontent.com/kcuilla/USgasprices/main/demo/gaspriceshinytour.gif" />
