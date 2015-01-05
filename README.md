metricsgraphics is an 'htmlwidget' interface to the MetricsGraphics.js D3 chart library.

The current `htmlwidget` wrapper for it is minimaly functional and does not provide support for metricsgraphics histograms and also does not really take advantage of metricsgraphics' best feature - time series charts. Both will be added in the next release and support for legends will appear shortly thereafter.

Charts look best in a Boostrap page (unless you customize your own CSS).

The following functions are implemented:

-   `mjs_plot`: Create a new metricsgraphics.js plot
-   `mjs_line`: metricsgraphics.js linechart "geom"
-   `mjs_point`: metricsgraphics.js scatterplot "geom"
-   `mjs_bar`: metricsgraphics.js bar chart "geom"
-   `mjs_axis_x`: Configure x axis ticks & limits
-   `mjs_axis_y`: Configure y axis ticks & limits
-   `mjs_labs`: Configure axis labels & plot description

### News

-   Version 0.1 released

### Installation

``` r
devtools::install_github("hrbrmstr/metricsgraphics")
```

### Usage

``` r
library(dplyr)
library(metricsgraphics)

tmp <- data.frame(year=seq(1790, 1970, 10), uspop=as.numeric(uspop))

tmp %>%
  mjs_plot(x=year, y=uspop) %>%
  mjs_line()

tmp %>%
  mjs_plot(x=year, y=uspop) %>%
  mjs_line(area=TRUE)

tmp %>% 
  mjs_plot(x=year, y=uspop) %>%
  mjs_bar()

mtcars %>% 
  mjs_plot(x=wt, y=mpg) %>%
  mjs_point(color_accessor=carb, size_accessor=carb) %>%
  mjs_labs(x="Weight of Car", y="Miles per Gallon")

mtcars %>% 
  mjs_plot(x=wt, y=mpg) %>%
  mjs_point(least_squares=TRUE) %>%
  mjs_labs(x="Weight of Car", y="Miles per Gallon")
```
