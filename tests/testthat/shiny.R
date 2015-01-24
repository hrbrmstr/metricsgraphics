

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
