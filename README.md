metricsgraphics is an 'htmlwidget' interface to the [MetricsGraphics.js](http://metricsgraphicsjs.org/) D3 chart library.

The current `htmlwidget` wrapper for it is functional but does not provide support for metricsgraphics histograms. It does provide basic support for metricsgraphics' best feature - time series charts.

Charts look best in a Boostrap page (unless you customize your own CSS).

You can see what the output below produces [on RPubs](http://rpubs.com/hrbrmstr/52782).

The following functions are implemented:

-   `mjs_plot`: Create a new metricsgraphics.js plot
-   `mjs_line`: metricsgraphics.js linechart "geom"
-   `mjs_add_line`: used to add additional columns for a multi-line chart
-   `mjs_add_legend`: adds a legend to a line (or mult-line) chart
-   `mjs_point`: metricsgraphics.js scatterplot "geom"
-   `mjs_bar`: metricsgraphics.js bar chart "geom"
-   `mjs_axis_x`: Configure x axis ticks & limits
-   `mjs_axis_y`: Configure y axis ticks & limits
-   `mjs_labs`: Configure axis labels & plot description
-   `mjs_add_baseline`: Sets a baseline line/label
-   `mjs_add_marker`: Sets a marker line/label

### News

-   Version 0.1 released
-   Version 0.2 released - added support for markers & baselines + minimal support for time-series
-   Version 0.3 released - coded up more config parameters (including color and point sizes) and added support for multi-line plots
-   Version 0.3.1 released - `mjs_marker` will now convert dates properly
-   Version 0.4 released - added `mjs_add_legend` to support legends in line/multi-line charts
-   Version 0.4.1 released - added support for linked charts (currently only works in `Rmd` files and mebbe Shiny if I can get more than one plot to show up in Shiny). See the [online Rmd demo](http://rpubs.com/hrbrmstr/52765) (scroll to bottom); also added some parameter error checking

### Installation

``` r
devtools::install_github("hrbrmstr/metricsgraphics")
```

### Usage

``` r
library(metricsgraphics)
library(RColorBrewer)

tmp <- data.frame(year=seq(1790, 1970, 10), uspop=as.numeric(uspop))

tmp %>%
  mjs_plot(x=year, y=uspop) %>%
  mjs_line() %>%
  mjs_add_marker(1850, "Something Wonderful") %>%
  mjs_add_baseline(150, "Something Awful")


tmp %>%
  mjs_plot(x=year, y=uspop, width=600) %>%
  mjs_line(area=TRUE)

tmp %>%
  mjs_plot(x=uspop, y=year, width=500, height=400) %>%
  mjs_bar() %>%
  mjs_axis_x(xax_format = 'plain')


mtcars %>%
  mjs_plot(x=wt, y=mpg, width=600, height=500) %>%
  mjs_point(color_accessor=carb, size_accessor=carb) %>%
  mjs_labs(x="Weight of Car", y="Miles per Gallon")


mtcars %>%
  mjs_plot(x=wt, y=mpg, width=600, height=500) %>%
  mjs_point(color_accessor=cyl,
            x_rug=TRUE, y_rug=TRUE,
            size_accessor=carb,
            size_range=c(5, 10),
            color_type="category",
            color_range=brewer.pal(n=11, name="RdBu")[c(1, 5, 11)]) %>%
  mjs_labs(x="Weight of Car", y="Miles per Gallon")


mtcars %>%
  mjs_plot(x=wt, y=mpg, width=400, height=300) %>%
  mjs_point(least_squares=TRUE) %>%
  mjs_labs(x="Weight of Car", y="Miles per Gallon")


set.seed(1492)
dat <- data.frame(date=seq(as.Date("2014-01-01"),
                           as.Date("2014-01-31"),
                           by="1 day"),
                  value=rnorm(n=31, mean=0, sd=2))

dat %>%
  mjs_plot(x=date, y=value) %>%
  mjs_line() %>%
  mjs_axis_x(xax_format = "date")

set.seed(1492)
stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4))

stocks %>%
  mjs_plot(x=time, y=X) %>%
  mjs_line() %>%
  mjs_axis_x(show=FALSE) %>%
  mjs_axis_y(show=FALSE)

stocks %>%
  mjs_plot(x=time, y=X) %>%
  mjs_line() %>%
  mjs_add_line(Y) %>%
  mjs_add_line(Z) %>%
  mjs_axis_x(xax_format="date")

library(shiny)
library(metricsgraphics)

ui = shinyUI(fluidPage(
  h3("MetricsGraphics Example", style="text-align:center"),
  metricsgraphicsOutput('mjs')
))

server = function(input, output) {
  output$mjs <- renderMetricsgraphics(
    mtcars %>% 
      mjs_plot(x=wt, y=mpg, width=400, height=300) %>%
      mjs_point(color_accessor=carb, size_accessor=carb) %>%
      mjs_labs(x="Weight of Car", y="Miles per Gallon")
  )
}

shinyApp(ui = ui, server = server)
```
