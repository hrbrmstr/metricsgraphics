[![Build Status](https://travis-ci.org/hrbrmstr/metricsgraphics.svg)](https://travis-ci.org/hrbrmstr/metricsgraphics) [![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active) ![downloads](http://cranlogs.r-pkg.org/badges/grand-total/metricsgraphics)

On CRAN: [<http://cran.r-project.org/web/packages/metricsgraphics/index.html>](http://cran.r-project.org/web/packages/metricsgraphics/index.html)

Vignette: [<http://cran.r-project.org/web/packages/metricsgraphics/vignettes/introductiontometricsgraphics.html>](http://cran.r-project.org/web/packages/metricsgraphics/vignettes/introductiontometricsgraphics.html)

============

metricsgraphics is an 'htmlwidget' interface to the [MetricsGraphics.js](http://metricsgraphicsjs.org/) D3 chart library.

Charts look best in a Boostrap page (unless you customize your own CSS).

You can see [core examples] (<http://rpubs.com/hrbrmstr/53741>) and [fairly extended grid example](http://rpubs.com/hrbrmstr/mjs_grid_07) on RPubs.

The following functions are implemented:

-   `mjs_plot`: Create a new metricsgraphics.js plot
-   `mjs_line`: metricsgraphics.js linechart "geom"
-   `mjs_add_line`: used to add additional columns for a multi-line chart
-   `mjs_hist`: Shortcut for plotting MetricsGraphics histograms
-   `mjs_histogram`: Plot Histograms with MetrisGraphics
-   `mjs_add_legend`: adds a legend to a line (or mult-line) chart
-   `mjs_point`: metricsgraphics.js scatterplot "geom"
-   `mjs_bar`: metricsgraphics.js bar chart "geom"
-   `mjs_axis_x`: Configure x axis ticks & limits
-   `mjs_axis_y`: Configure y axis ticks & limits
-   `mjs_labs`: Configure axis labels & plot description
-   `mjs_add_baseline`: Sets a baseline line/label
-   `mjs_add_marker`: Sets a marker line/label
-   `mjs_grid`: `grid.arrange`-like functionality for `metricsgraphics` charts
-   `mjs_add_mouseover`: provides support for MetricsGraphics [custom rollovers](https://github.com/mozilla/metrics-graphics/wiki/Graphic#mouseover)
-   `mjs_add_confidence_band`: provides support for confidence bands

### News

-   Version 0.1 released
-   Version 0.2 released - added support for markers & baselines + minimal support for time-series
-   Version 0.3 released - coded up more config parameters (including color and point sizes) and added support for multi-line plots
-   Version 0.3.1 released - `mjs_marker` will now convert dates properly
-   Version 0.4 released - added `mjs_add_legend` to support legends in line/multi-line charts
-   Version 0.4.1 released - added support for linked charts (currently only works in `Rmd` files and mebbe Shiny if I can get more than one plot to show up in Shiny). See the [online Rmd demo](http://rpubs.com/hrbrmstr/52765) (scroll to bottom); also added some parameter error checking
-   Version 0.5 released - added histograms (`mjs_histogram` & `mjs_hist`)
-   Version 0.6 relased - added `mjs_grid` for `grid.arrange`-like functionality for placing multiple charts (ref: [<http://rpubs.com/hrbrmstr/metricsgraphics0-6>](http://rpubs.com/hrbrmstr/metricsgraphics0-6))
-   Version 0.6.1 released - Fixed bug that broke widget in new shiny/shinydashboard context
-   Version 0.7 released - Updated widget for MetricsGraphics 2.1.0; added `mjs_add_mouseover` and tweaked `mjs_bar`. NOTE: As the Mozilla folks said, the bar charts API is quite unstable. There won't be much effort to support them in this package until the API stabilizes a bit more.
-   Version 0.7.5 released - updated all functions that take bare inputs and made them flexible enough to take bare or quoted inputs.
-   Version 0.8.0 released - updated `mjs_rollover` to show scatterplot example and cleaned up some of the base code & package layout for an attempt at a CRAN release. Also updated MetricsGraphics.js to latest release version (2.5)
-   Version 0.8.5 released - enhanced & changed some of the package functions to match MG's new javascript interface. Added `mjs_add_confidence_band`. Added package vignette.

### Installation

``` r
# stable
install.pacakges("metricsgraphics")
# development
# devtools::install_github("hrbrmstr/metricsgraphics")
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

# Custom rollovers

dat %>%
  mjs_plot(x=date, y=value) %>%
  mjs_line() %>%
  mjs_axis_x(xax_format = "date") %>%
  mjs_add_mouseover("function(d, i) {
                $('{{ID}} svg .mg-active-datapoint')
                    .text('custom text : ' + d.date + ' ' + i);
                 }")

# also works for scatterplots with a slight mod

set.seed(1492)
dat <- data.frame(value=rnorm(n=30, mean=5, sd=1),
                 value2=rnorm(n=30, mean=4, sd=1),
                 test = c(rep(c('test', 'test2'), 15)))
dat %>%
 mjs_plot(x = value, y = value2) %>%
 mjs_point() %>%
 mjs_add_mouseover("function(d, i) {
               $('{{ID}} svg .mg-active-datapoint')
                   .text('custom text : ' + d.point.test + ' ' + i);
                }")

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

mjs_plot(rnorm(10000)) %>%
  mjs_histogram(bins=30, bar_margin=1)

movies <- ggplot2::movies[sample(nrow(ggplot2::movies), 1000), ]

mjs_plot(movies$rating) %>% mjs_histogram()

mjs_plot(movies, rating) %>% 
  mjs_histogram() %>% 
  mjs_labs(x_label="Histogram of movie ratings", 
           y_label="Frequency")

mjs_plot(movies$rating) %>% mjs_histogram(bins=30)

mjs_plot(runif(10000)) %>% 
  mjs_labs(x_label="runif(10000)") %>%
  mjs_histogram()


mjs_plot(rbeta(10000, 2, 5)) %>%
  mjs_labs(x_label="rbeta(10000, 2, 3)") %>%
  mjs_histogram(bins=100) %>% 
  mjs_axis_y(extended_ticks=TRUE)

bimod <- c(rnorm(1000, 0, 1), rnorm(1000, 3, 1))
mjs_plot(bimod) %>% mjs_histogram() 
mjs_plot(bimod) %>% mjs_histogram(bins=30) 

bimod %>% mjs_hist(30)

library(shiny)
library(metricsgraphics)

ui = shinyUI(fluidPage(
  h3("MetricsGraphics Example", style="text-align:center"),
  metricsgraphicsOutput('mjs1'),
  br(),
  metricsgraphicsOutput('mjs2')
))

server = function(input, output) {

  mtcars %>%
    mjs_plot(x=wt, y=mpg, width=400, height=300) %>%
    mjs_point(color_accessor=carb, size_accessor=carb) %>%
    mjs_labs(x="Weight of Car", y="Miles per Gallon") -> m1

  set.seed(1492)
  stocks <- data.frame(
    time = as.Date('2009-01-01') + 0:9,
    X = rnorm(10, 0, 1),
    Y = rnorm(10, 0, 2),
    Z = rnorm(10, 0, 4))

  stocks %>%
    mjs_plot(x=time, y=X) %>%
    mjs_line() %>%
    mjs_add_line(Y) %>%
    mjs_add_line(Z) %>%
    mjs_axis_x(xax_format="date") %>%
    mjs_add_legend(legend=c("X", "Y", "Z")) -> m2

  output$mjs1 <- renderMetricsgraphics(m1)

  output$mjs2 <- renderMetricsgraphics(m2)

}

shinyApp(ui = ui, server = server)
```
