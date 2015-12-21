#' Adds a legend to a metricsgraphics chart
#'
#' @param mjs metricsgraphics plot object
#' @param legend character vector of labels for the legend
#' @param inline \code{TRUE} if you want line labes to the right of the chart
#'        vs in a legend block (experimental)
#' @export
#' @return metricsgraphics object
#' @examples
#' set.seed(1492)
#' stocks <- data.frame(
#'   time = as.Date('2009-01-01') + 0:9,
#'   X = rnorm(10, 0, 1),
#'   Y = rnorm(10, 0, 2),
#'   Z = rnorm(10, 0, 4))
#'
#' stocks %>%
#'   mjs_plot(x=time, y=X) %>%
#'   mjs_line() %>%
#'   mjs_add_line(Y) %>%
#'   mjs_add_line(Z) %>%
#'   mjs_axis_x(xax_format="date") %>%
#'   mjs_add_legend(legend=c("X", "Y", "Z"))
mjs_add_legend <- function(mjs, legend, inline=FALSE) {
  mjs$x$legend <- legend
  if (!inline) mjs$x$legend_target <- sprintf("#%s-legend", mjs$elementId)
  mjs
}
