#' metricsgraphics.js linechart "geom"
#'
#' This function adds a line "geom" to a metricsgraphics.js html widget.
#'
#' @param mjs metricsgraphics plot object
#' @param area fill in area under line? (default: \code{FALSE} - no)
#' @param animate_on_load animate the drawing of the plot on page load? (default: \code{FALSE} - no)
#' @param color line color (hex string or valid HTML color string). Use \code{NULL} (the default)
#'        to use the default Metrics Graphics colors or if you plan on controlling the colors with CSS.
#' @param interpolate the interpolation function to use when rendering lines.
#'        possible values: ("cardinal", "linear", "linear-closed", "step", "step-before",
#'        "step-after", "basis", "basis-open", "basis-closed", "bundle", "cardinal-open",
#'        "cardinal-closed", "monotone", "basic")
#' @return metricsgraphics object
#' @note If you plan on using cusom colors, all lines must have a color value or the result is
#'       non-deterministic.
#' @export
#' @examples
#' data.frame(year=seq(1790, 1970, 10),
#'            uspop=as.numeric(uspop)) %>%
#'   mjs_plot(x=year, y=uspop) %>%
#'   mjs_line()
#'
mjs_line <- function(mjs,
                     area=FALSE, animate_on_load=FALSE,
                     color=NULL,
                     interpolate="cardinal") {

  if(!interpolate %in% c("cardinal", "linear", "linear-closed", "step",
                          "step-before", "step-after", "basis", "basis-open",
                          "basis-closed", "bundle", "cardinal-open",
                          "cardinal-closed", "monotone", "basic")) {
    stop("'interpolate' must be a valid value")
  }


  mjs$x$area <- area
  mjs$x$animate_on_load <- animate_on_load
  mjs$x$geom <- "line"
  mjs$x$color <- color
  mjs$x$colors <- color
  mjs$x$interpolate <- interpolate
  mjs
}

#' Add a new line to a metricsgraphics.js linechart "geom"
#'
#' This function adds a line to an existing \code{mjs_line} "geom". Specify
#' the bare or quoted name of the column to use in \code{y_accessor} and it will be added
#' to the plot.
#'
#' @note You must have called \code{mjs_line} first before adding additional columns. If you plan on
#'       using cusom colors, all lines must have a color value or the result is non-deterministic.
#' @param mjs metricsgraphics plot object
#' @param y_accessor bare or quoted name of column to add to the existing line plot
#' @param color line color. Use \code{NULL} (the default) to use default Metrics Graphics colors
#'        or if you plan on using CSS to control the colors.
#' @return metricsgraphics object
#' @export
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
#'   mjs_axis_x(xax_format="date")
mjs_add_line <- function(mjs, y_accessor, color=NULL) {

  y_accessor <- substitute(y_accessor)
  if (inherits(y_accessor, "name")) { y_accessor <- as.character(y_accessor) }

  multi_line <- mjs$x$multi_line
  if (is.null(multi_line)) multi_line <- list()
  new_line <- y_accessor
  multi_line <- c(multi_line, new_line)
  mjs$x$multi_line <- multi_line
  mjs$x$color <- NULL
  if (!is.null(color)) { mjs$x$colors <- c(mjs$x$colors, color) }
  mjs

}
