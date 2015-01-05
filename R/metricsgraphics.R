#' Create a new metricsgraphics.js  plot
#'
#' \code{mjs_plot()} initializes the metricsgraphics.js html widget
#' and takes a data frame & (bare) x & y column names as minimum input.
#' This must be piped to a "geom" (metricsgraphics.js only supports single
#' "geom" layers) and can also be piped to other \code{mjs_} functions that
#' manipulate aesthetics.
#'
#' See \href{http://metricsgraphicsjs.org/}{MetricsGraphics.js} for more information.
#'
#' @param data data frame
#' @param x bare name of column to use for x values
#' @param y bare name of column to use for y values
#' @param left the size of the left margin in pixels.
#' @param right the size of the right margin in pixels.
#' @param top the size of the top margin in pixels.
#' @param bottom the size of the bottom margin in pixels.
#' @param buffer the buffer size in pixels between the actual chart area and the margins.
#' @param width Width in pixels (optional, defaults to automatic sizing)
#' @param height Height in pixels (optional, defaults to automatic sizing)
#' @import htmlwidgets
#' @export
#' @examples \dontrun{
#' data.frame(year=seq(1790, 1970, 10),
#'            uspop=as.numeric(uspop)) %>%
#'   mjs_plot(x=year, y=uspop) %>%
#'   mjs_line()
#' }
#'
mjs_plot <- function(data, x, y,
                     left = 80, right = 10,
                     top = 40, bottom = 60, buffer = 8,
                     width = NULL, height = NULL) {

  params = list(
    data=data,
    chart_type="line",
    xax_format="plain",
    x_label=NULL,
    y_label=NULL,
    title=NULL,
    description=NULL,
    left=left,
    right=right,
    bottom=bottom,
    buffer=buffer,
    y_scale_type="linear",
    yax_count=5,
    xax_count=6,
    x_rug=FALSE,
    y_rug=FALSE,
    area=FALSE,
    size_accessor=NULL,
    color_accessor=NULL,
    bar_height=20,
    bar_margin=1,
    bins=NULL,
    binned=TRUE,
    min_x=NULL,
    max_x=NULL,
    min_y=NULL,
    max_y=NULL,
    least_squares=FALSE,
    x_accessor=substitute(x),
    y_accessor=substitute(y),
    geom="line"
  )

  htmlwidgets::createWidget(
    name = 'metricsgraphics',
    x = params,
    width = width,
    height = height,
    package = 'metricsgraphics'
  )

}

#' metricsgraphics.js bar chart "geom"
#'
#' This function adds a bar "geom" to a metricsgraphics.js html widget.
#'
#' @param mjs plot object
#' @param bar_height width of bars
#' @param binned is data already binned? (default: \code{TRUE} - yes)
#' @note metricsgraphics.js currently has "meh" support for bar charts
#' @export
#' @examples \dontrun{
#' data.frame(year=seq(1790, 1970, 10),
#'            uspop=as.numeric(uspop)) %>%
#'   mjs_plot(x=year, y=uspop, width=300, height=400)
#'   mjs_bar()
#' }
#'
mjs_bar <- function(mjs,
                    bar_height=20, binned=TRUE) {
  mjs$x$chart_type <- "bar"
  mjs$x$bar_height <- bar_height
  mjs$x$binned <- binned
  mjs$x$geom <- "bar"
  mjs
}



#' metricsgraphics.js linechart "geom"
#'
#' This function adds a line "geom" to a metricsgraphics.js html widget.
#'
#' @param mjs plot object
#' @param area fill in area under line? (default: \code{FALSE} - no)
#' @param animate_on_load animate the drawing of the plot on page load? (default: \code{FALSE} - no)
#' @export
#' @examples \dontrun{
#' data.frame(year=seq(1790, 1970, 10),
#'            uspop=as.numeric(uspop)) %>%
#'   mjs_plot(x=year, y=uspop) %>%
#'   mjs_line()
#' }
#'
mjs_line <- function(mjs,
                     area=FALSE, animate_on_load=FALSE) {
  mjs$x$area <- area
  mjs$x$animate_on_load <- animate_on_load
  mjs$x$geom <- "line"
  mjs
}

#' metricsgraphics.js scatterplot "geom"
#'
#' This function adds a point/scatterplot "geom" to a metricsgraphics.js html widget.
#'
#' @param mjs plot object
#' @param least_squares add a least squares line? (default: \code{FALSE} - no)
#' @param size_accessor bare name of a column to use to scale the size of the points
#' @param color_accessor bare name of a column to use to scale the color of the points
#' @param x_rug show a "rug" plot next to the x axis? (default: \code{FALSE} - no)
#' @param y_rug show a "rug" plot next to the y axis? (default: \code{FALSE} - no)
#' @export
#' @examples \dontrun{
#' mtcars %>%
#'  mjs_plot(x=wt, y=mpg, width=400, height=300) %>%
#'  mjs_point(least_squares=TRUE)
#' }
#'
mjs_point <- function(mjs,
                      least_squares=FALSE,
                      size_accessor=NULL,
                      color_accessor=NULL,
                      x_rug=FALSE,
                      y_rug=FALSE) {
  mjs$x$chart_type <- "point"
  mjs$x$least_squares<- least_squares
  mjs$x$x_rug <- x_rug
  mjs$x$y_rug <- y_rug
  mjs$x$size_accessor <- substitute(size_accessor)
  mjs$x$color_accessor <- substitute(color_accessor)
  mjs$x$geom <- "point"
  mjs
}

#' Configure axis labels & plot description
#'
#' @export
#' @examples \dontrun{
#' mtcars %>%
#'  mjs_plot(x=wt, y=mpg, width=400, height=300) %>%
#'  mjs_point(color_accessor=carb, size_accessor=carb) %>%
#'  mjs_labs(x="Weight of Car", y="Miles per Gallon")
#' }
#'
mjs_labs <- function(mjs,
                     x_label=NULL, y_label=NULL) {
  mjs$x$x_label <- x_label
  mjs$x$y_label <- y_label
  mjs
}

#' Configure x axis ticks & limits
#'
#' @param mjs plot object
#' @param xax_count tick count
#' @param min_x min limit for x axis
#' @param max_x max limit for x axis
#' @param xax_format how to format tick labels. Currently one of "plain", "comma" or "date"
#' @note xax_format is likely to undergo a drastic change in future releases but support for these three formats will also likely remain.
#' @export
mjs_axis_x <- function(mjs,
                       xax_count=6,
                       min_x=NULL, max_x=NULL,
                       xax_format="plain") {
  mjs$x$xax_count <- xax_count
  mjs
}

#' Configure y axis ticks & limits
#'
#' @param mjs plot object
#' @param yax_count tick count
#' @param min_y min limit for y axis
#' @param max_y max limit for y axis
#' @param y_scale_type scale for y axis; either "linear" (default) or "log"
#' @export
#' @export
mjs_axis_y <- function(mjs,
                       yax_count=5,
                       min_y=NULL, max_y=NULL,
                       y_scale_type="linear") {
  mjs$x$yax_count <- yax_count
  mjs$x$y_scale_type <- y_scale_type
  mjs
}

#' Widget output function for use in Shiny
#'
#' @export
metricsgraphicsOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'metricsgraphics', width, height, package = 'metricsgraphics')
}

#' Widget render function for use in Shiny
#'
#' @export
renderMetricsgraphics <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, metricsgraphicsOutput, env, quoted = TRUE)
}
