context("basic functionality")
test_that("we can do something", {

  tmp <- data.frame(year=seq(1790, 1970, 10),
                    uspop=as.numeric(uspop))

  tmp %>%
    mjs_plot(x=year, y=uspop) %>%
    mjs_line() %>%
    mjs_add_marker(1850, "Something Wonderful") %>%
    mjs_add_baseline(150, "Something Awful") -> mj1

  mjs_plot(rnorm(10000)) %>%
    mjs_histogram(bins=30, bar_margin=1) -> mj2

  x <- "wt"
  y <- "mpg"

  mtcars %>%
    mjs_plot(x=x, y=y, width=600, height=500) %>%
    mjs_point(color_accessor=carb, size_accessor=carb) %>%
    mjs_labs(x="Weight of Car", y="Miles per Gallon") -> mj3

  expect_that(mj1, is_a("metricsgraphics"))
  expect_that(mj2, is_a("metricsgraphics"))
  expect_that(mj3, is_a("metricsgraphics"))

  expect_that(mj1$x$chart_type, equals("line"))
  expect_that(mj2$x$chart_type, equals("histogram"))
  expect_that(mj3$x$chart_type, equals("point"))

})
