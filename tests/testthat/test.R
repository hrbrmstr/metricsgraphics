library(metricsgraphics)
library(RColorBrewer)

tmp <- data.frame(year=seq(1790, 1970, 10), uspop=as.numeric(uspop))

tmp %>%
  mjs_plot(x=year, y=uspop) %>%
  mjs_line() %>%
  mjs_add_marker(1850, "Something Wonderful") %>%
  mjs_add_baseline(150, "Something Awful")


mjs_plot(rnorm(10000)) %>%
  mjs_histogram(bins=30, bar_margin=1)

movies <- ggplot2movies::movies[sample(nrow(ggplot2movies::movies), 1000), ]

mjs_plot(movies$rating) %>% mjs_histogram()

tmp %>%
  mjs_plot(x=year, y=uspop, width=600) %>%
  mjs_line(area=TRUE)

tmp %>%
  mjs_plot(x=uspop, y=year, width=500, height=400) %>%
  mjs_bar() %>%
  mjs_axis_x(xax_format = 'plain')


x <- "wt"
y <- "mpg"

mtcars %>%
  mjs_plot(x=x, y=y, width=600, height=500) %>%
  mjs_point(color_accessor=carb, size_accessor=carb) %>%
  mjs_labs(x="Weight of Car", y="Miles per Gallon")

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
  mjs_labs(x="Weight of Car", y="Miles per Gallon") %>%
  mjs_add_legend(legend="X")


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

dat %>%
  mjs_plot(x=date, y=value) %>%
  mjs_line() %>%
  mjs_axis_x(xax_format = "date") %>%
  mjs_add_mouseover("function(d, i) {
                $('{{ID}} svg .mg-active-datapoint')
                    .text('custom text : ' + d.date + ' ' + i);
                 }")

set.seed(1492)
stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4))


set.seed(1999)
stocks2 <- data.frame(
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
  mjs_add_legend(legend=c("X", "Y", "Z"))


stocks %>%
  mjs_plot(x=time, y=X) %>%
  mjs_line() %>%
  mjs_axis_x(show=FALSE) %>%
  mjs_axis_y(show=FALSE)
