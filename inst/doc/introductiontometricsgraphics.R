## ----echo=FALSE----------------------------------------------------------
suppressPackageStartupMessages(library(metricsgraphics))
suppressPackageStartupMessages(library(jsonlite))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(htmltools))
suppressPackageStartupMessages(library(dplyr))

## ------------------------------------------------------------------------
library(metricsgraphics)
library(jsonlite)
library(RColorBrewer)
library(htmltools)
library(dplyr)

# this lets us add a title to the plot since the package follows the guidance
# of the htmlwidgets authors and does not include the MetricsGraphics.js title
# option to ensure consistent div sizing.

show_plot <- function(plot_object, title) {
  div(style="margin:auto;text-align:center", strong(title), br(), plot_object)
}

## ------------------------------------------------------------------------
fake_users_1 <- fromJSON("http://metricsgraphicsjs.org/data/fake_users1.json")
fake_users_1$date <- as.Date(fake_users_1$date)

fake_users_1 %>%
  mjs_plot(x=date, y=value, width=600, height=200) %>%
  mjs_axis_x(xax_format="date") %>% 
  mjs_line(area=TRUE) %>% 
  show_plot("Line Chart")


## ------------------------------------------------------------------------
confidence_band <- fromJSON("http://metricsgraphicsjs.org/data/confidence_band.json")

confidence_band %>%
  mjs_plot(x=date, y=value, format="percentage", width=600, height=200) %>%
  mjs_axis_x(xax_format="date", 
             show_secondary_x_label=FALSE, 
             extended_ticks=TRUE) %>% 
  mjs_line() %>% 
  mjs_add_confidence_band() %>% 
  show_plot("Confidence Band")

## ------------------------------------------------------------------------
small_range <- fromJSON("http://metricsgraphicsjs.org/data/small-range.json")

small_range %>% 
  mjs_plot(x=date, y=value, width=600, height=200) %>%
  mjs_axis_x(xax_format="date") %>% 
  mjs_line(interpolate="basic", area=TRUE) %>% 
  show_plot("Small Range of Integers")
  

## ------------------------------------------------------------------------
brief_1 <- fromJSON("http://metricsgraphicsjs.org/data/brief-1.json")
brief_2 <- fromJSON("http://metricsgraphicsjs.org/data/brief-2.json")

brief_1 %>% 
  mjs_plot(x=date, y=value, width=600, height=200, linked=TRUE) %>%
  mjs_axis_x(xax_format="date", xax_count=4) %>% 
  mjs_line(area=TRUE) -> mjs_brief_1

brief_2 %>% 
  mjs_plot(x=date, y=value, width=600, height=200, linked=TRUE) %>%
  mjs_axis_x(xax_format="date", xax_count=4) %>% 
  mjs_line() -> mjs_brief_2

div(style="margin:auto;text-align:center", 
    strong("Linked Graphic"), br(), mjs_brief_1,
    strong("Other Linked Graphic"), br(), mjs_brief_2)

## ------------------------------------------------------------------------
solitary <- data.frame(
  date=as.Date("2015-03-05"),
  value=12000
)

solitary %>% 
  mjs_plot(x=date, y=value, width=600, height=200) %>%
  mjs_axis_x(xax_format="date") %>% 
  mjs_point() %>% 
  show_plot("Singleton")

## ------------------------------------------------------------------------
fake_users2_list <- fromJSON("http://metricsgraphicsjs.org/data/fake_users2.json")
fake_users2 <- data.frame(
  date=fake_users2_list[[1]]$date,
  value_1=fake_users2_list[[1]]$value,
  value_2=fake_users2_list[[2]]$value,
  value_3=fake_users2_list[[3]]$value
)

fake_users2 %>% 
  mjs_plot(x=date, y=value_1, width=600, height=200) %>%
  mjs_axis_x(xax_format="date") %>% 
  mjs_line() %>% 
  mjs_add_line(value_2) %>% 
  mjs_add_line(value_3) %>% 
  mjs_add_legend(c("Line 1", "Line 2", "Line 3")) %>% 
  show_plot("Multi-Line Chart")

fake_users2 %>% 
  mjs_plot(x=date, y=value_1, width=600, height=200) %>%
  mjs_axis_x(xax_format="date") %>% 
  mjs_line(color="blue") %>% 
  mjs_add_line(value_2, color="rgb(255,100,43)") %>% 
  mjs_add_line(value_3, color="#ccccff") %>% 
  mjs_add_legend(c("Line 1", "Line 2", "Line 3")) %>% 
  show_plot("Multi-Line Char with Custom Colors")

## ------------------------------------------------------------------------
fake_users3_list <- fromJSON("http://metricsgraphicsjs.org/data/fake_users3.json")
fake_users3 <- data.frame(
  date=fake_users3_list[[1]]$date,
  value_1=fake_users3_list[[1]]$value,
  value_2=fake_users3_list[[2]]$value,
  value_3=fake_users3_list[[3]]$value
)

fake_users3 %>% 
  mjs_plot(x=date, y=value_1, width=600, height=200, right=40) %>%
  mjs_axis_x(xax_format="date") %>% 
  mjs_line() %>% 
  mjs_add_line(value_2) %>% 
  mjs_add_line(value_3) %>% 
  mjs_add_legend(c('US', 'CA', 'DE'), inline=TRUE) %>% 
  show_plot("Labeling Lines")

## ------------------------------------------------------------------------
xnotondate <- fromJSON("http://metricsgraphicsjs.org/data/xnotdate.json")

xnotondate %>% 
  mjs_plot(x=males, y=females, width=600, height=240, 
           left=80, right=40, bottom=50) %>% 
  mjs_line(animate_on_load=TRUE, area=FALSE) %>% 
  mjs_labs("Males", "Females") %>% 
  mjs_axis_y(extended_ticks=TRUE) %>% 
  show_plot("Axis Labels")

## ------------------------------------------------------------------------
some_percentages <- fromJSON("http://metricsgraphicsjs.org/data/some_percentage.json")

some_percentages[[1]] %>% 
  mjs_plot(x=date, y=value, format="percentage", width=600, height=200) %>% 
  mjs_axis_x(xax_format="date") %>% 
  mjs_line(area=TRUE) %>% 
  show_plot("Some Percentages")

## ------------------------------------------------------------------------
some_currency <- fromJSON("http://metricsgraphicsjs.org/data/some_currency.json")

some_currency %>% 
  mjs_plot(x=date, y=value, width=600, height=200) %>% 
  mjs_axis_x(xax_format="date") %>% 
  mjs_line() %>% 
  mjs_axis_y(yax_units="$") %>% 
  show_plot("Some Currency")

## ------------------------------------------------------------------------
log_scale <- fromJSON("http://metricsgraphicsjs.org/data/log.json")

log_scale %>% 
  mjs_plot(x=date, y=value, width=600, height=200) %>% 
  mjs_axis_x(xax_format="date") %>% 
  mjs_line(area=TRUE) %>% 
  mjs_axis_y(y_scale_type="log") %>% 
  show_plot("Log Scale")

## ------------------------------------------------------------------------
fake_users_1 <- fromJSON("http://metricsgraphicsjs.org/data/fake_users1.json")
brief_1 <- fromJSON("http://metricsgraphicsjs.org/data/brief-1.json")

fake_users_1 %>% 
  mjs_plot(x=date, y=value, width=600, height=200) %>% 
  mjs_axis_x(xax_format="date", show=FALSE) %>% 
  mjs_line() -> no_x
  
brief_1 %>% 
  mjs_plot(x=date, y=value, width=600, height=200) %>% 
  mjs_axis_x(xax_format="date") %>%
  mjs_axis_y(show=FALSE) %>% 
  mjs_line() -> no_y

div(style="margin:auto;text-align:center", 
    strong("No X Axis"), br(), no_x,
    strong("No Y Axis"), br(), no_y)

## ------------------------------------------------------------------------
fake_users_1 <- fromJSON("http://metricsgraphicsjs.org/data/fake_users1.json")

fake_users_1 %>% 
  mjs_plot(x=date, y=value, width=600, height=200) %>% 
  mjs_axis_x(xax_format="date", show=FALSE) %>% 
  mjs_line(color="#8c001a", area=TRUE) %>% 
  mjs_axis_y(rug=TRUE) %>% 
  show_plot("Colors!")

## ------------------------------------------------------------------------
fake_users_1 <- fromJSON("http://metricsgraphicsjs.org/data/fake_users1.json")

fake_users_1 %>% 
  mjs_plot(x=date, y=value, width=600, height=200) %>% 
  mjs_axis_x(xax_format="date", show=FALSE) %>% 
  mjs_line() %>% 
  mjs_axis_y(rug=TRUE) %>% 
  show_plot("Rug Plots")

## ------------------------------------------------------------------------
some_percentages <- fromJSON("http://metricsgraphicsjs.org/data/some_percentage.json")

some_percentages[[1]] %>% 
  mjs_plot(x=date, y=value, format="percentage", width=600, height=200) %>% 
  mjs_axis_x(xax_format="date") %>% 
  mjs_line(area=TRUE) %>% 
  mjs_add_marker("2014-02-01", "1st Milestone") %>% 
  mjs_add_marker(as.Date("2014-03-15"), "2nd Milestone") %>% 
  show_plot("Markers")

## ------------------------------------------------------------------------
fake_users_1 <- fromJSON("http://metricsgraphicsjs.org/data/fake_users1.json")

fake_users_1 %>% 
  mjs_plot(x=date, y=value, width=600, height=200) %>% 
  mjs_axis_x(xax_format="date", show=FALSE) %>% 
  mjs_add_baseline(160000000, "a baseline") %>% 
  mjs_line(area=TRUE) %>% 
  show_plot("Baselines")

## ------------------------------------------------------------------------
points_1 <- fromJSON("http://metricsgraphicsjs.org/data/points1.json")

points_1 %>% 
  mjs_plot(x=x, y=y, width=600, height=460) %>% 
  mjs_point(y_rug=TRUE) %>% 
  mjs_axis_x() %>% 
  show_plot("Simple Scatterplot")

## ------------------------------------------------------------------------
points_1 %>% 
  mjs_plot(x=x, y=y, width=600, height=460) %>% 
  mjs_point(y_rug=TRUE, color_accessor=v, color_type="category", color_range=c("green", "orange")) %>% 
  mjs_axis_x() %>% 
  show_plot("Color mapping")

## ------------------------------------------------------------------------
points_1 %>% 
  mjs_plot(x=x, y=y, width=600, height=460) %>% 
  mjs_point(y_rug=TRUE, x_rug=TRUE, color_accessor=z, size_accessor=w, color_type="category") %>% 
  mjs_axis_x(rug=TRUE) %>% 
  show_plot("Size Too!")

## ------------------------------------------------------------------------
moar_plots <- lapply(1:7, function(x) {
  mjs_plot(rbeta(10000, x, x), width="250px", height="250px", linked=TRUE) %>%
    mjs_histogram(bar_margin=2) %>%
    mjs_labs(x_label=sprintf("Plot %d", x))
})

mjs_grid(moar_plots, nrow=4, ncol=3, widths=c(rep(0.33, 3)))

