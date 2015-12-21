#' Adds a custom rollover to a metricsgraphics chart
#'
#' MetricsGraphics charts allow for \href{https://github.com/mozilla/metrics-graphics/wiki/Graphic#mouseover}{custom rollovers}.
#' \code{mjs_add_mouseover} lets you add a custom rollover to a metricsgraphics object. You must be
#' familiar with javascript and D3 idioms since you are supplying a javascript function as
#' a parameter.\cr
#' \cr
#' Since targeting is done by element id, you will need to add a special string - \code{\{\{ID\}\}} -
#' to the target element selector so metricsgraphics can add the unique object identifier
#' to the selector. See Examples for basic usage.
#'
#' @param mjs metricsgraphics plot object
#' @param func text for javascript function to be used for the custom rollover. See Details for usage.
#' @export
#' @return metricsgraphics object
#' @note you need to use \code{d.point.THING} vs \code{d.THING} when trying to add mouseovers to a
#'     metricsgraphics scatterplot.
#' @export
#' @examples
#' set.seed(1492)
#' dat <- data.frame(date=as.Date('2009-01-01') + 0:9,
#'                   value=rnorm(10, 0, 2))
#' dat %>%
#'   mjs_plot(x=date, y=value) %>%
#'   mjs_line() %>%
#'   mjs_axis_x(xax_format = "date") %>%
#'   mjs_add_mouseover("function(d, i) {
#'                 $('{{ID}} svg .mg-active-datapoint')
#'                     .text('custom text : ' + d.date + ' ' + i);
#'                  }")
#'
#' # slightly different for scatterplots
#'
#' dat <- data.frame(value=rnorm(n=30, mean=5, sd=1),
#'                   value2=rnorm(n=30, mean=4, sd=1),
#'                   test = c(rep(c('test', 'test2'), 15)))
#'
#' dat %>%
#'   mjs_plot(x = value, y = value2) %>%
#'   mjs_point() %>%
#'   mjs_add_mouseover("function(d, i) {
#'                 $('{{ID}} svg .mg-active-datapoint')
#'                     .text('custom text : ' + d.point.test + ' ' + i);
#'                  }")
#'
mjs_add_mouseover <- function(mjs, func) {
  mjs$x$mouseover <- JS(gsub("\\{\\{ID\\}\\}", sprintf("%s", mjs$x$target), func))
  mjs
}
