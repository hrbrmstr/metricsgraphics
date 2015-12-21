#' metricsgraphics.js bar chart "geom"
#'
#' This function adds a bar "geom" to a metricsgraphics.js html widget.
#'
#' @param mjs metricsgraphics plot object
#' @param bar_height width of bars
#' @param binned is data already binned? (default: \code{TRUE} - yes)
#' @return metricsgraphics object
#' @note metricsgraphics.js currently has "meh" support for bar charts
#' @export
#' @examples
#' data.frame(year=seq(1790, 1970, 10),
#'            uspop=as.numeric(uspop)) %>%
#'   mjs_plot(x=year, y=uspop, width=300, height=400) %>%
#'   mjs_bar()
#'
mjs_bar <- function(mjs,
                    bar_height=20, binned=TRUE) {
  mjs$x$data[, mjs$x$y_accessor] <- as.character(mjs$x$data[, mjs$x$y_accessor])
  mjs$x$chart_type <- "bar"
  mjs$x$bar_height <- bar_height
  mjs$x$binned <- binned
  mjs$x$geom <- "bar"
  mjs
}
