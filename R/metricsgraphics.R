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
#' @param show_rollover_text determines whether or not to show any text when a data point is rolled over.
#' @param linked inks together all other graphs whose linked option is set to true. When one graphic in that set is rolled over, the corresponding values in the other graphics are also rolled over (default: \code{FALSE} - not linked)
#' @param decimals the number of decimals to show in a rollover (default: \code{2})
#' @param format sets the format of the data object, which is to say, counts or percentages
#' @param missing_is_zero if true and if the data object is a time series, missing data points will be treated as zeros
#' @param left the size of the left margin in pixels.
#' @param right the size of the right margin in pixels.
#' @param top the size of the top margin in pixels.
#' @param bottom the size of the bottom margin in pixels.
#' @param buffer the buffer size in pixels between the actual chart area and the margins.
#' @param width Width in pixels (optional, defaults to automatic sizing)
#' @param height Height in pixels (optional, defaults to automatic sizing)
#' @import htmlwidgets htmltools
#' @export
#' @examples \dontrun{
#' data.frame(year=seq(1790, 1970, 10),
#'            uspop=as.numeric(uspop)) %>%
#'   mjs_plot(x=year, y=uspop) %>%
#'   mjs_line()
#' }
#'
mjs_plot <- function(data, x, y,
                     show_rollover_text = TRUE,
                     linked = FALSE,
                     decimals=2, format="count",
                     missing_is_zero=FALSE,
                     left = 80, right = 10,
                     top = 40, bottom = 60, buffer = 8,
                     width = NULL, height = NULL) {

  stopifnot(format %in% c("percentage", "count"))

  eid <- sprintf("mjs-%s", paste(sample(c(letters[1:6],0:9),30,replace=TRUE),collapse=""))

  params = list(
    data=data,
    x_axis=TRUE,
    y_axis=TRUE,
    show_confidence_band=NULL,
    chart_type="line",
    xax_format="plain",
    x_label=NULL,
    y_label=NULL,
    markers=NULL,
    baselines=NULL,
    linked=linked,
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
    missing_is_zero=missing_is_zero,
    size_accessor=NULL,
    color_accessor=NULL,
    color_type="number",
    color_range=c("blue", "red"),
    size_range=c(1, 5),
    bar_height=20,
    bar_margin=1,
    bins=NULL,
    binned=TRUE,
    min_x=NULL,
    max_x=NULL,
    min_y=NULL,
    max_y=NULL,
    least_squares=FALSE,
    interpolate="cardinal",
    decimals=decimals,
    show_rollover_text=show_rollover_text,
    x_accessor=substitute(x),
    y_accessor=substitute(y),
    multi_line=NULL,
    geom="line",
    legend=NULL,
    legend_target=NULL,
    target=sprintf("#%s", eid)
  )

  htmlwidgets::createWidget(
    name = 'metricsgraphics',
    x = params,
    width = width,
    height = height,
    package = 'metricsgraphics',
    elementId = eid
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
#' @param interpolate the interpolation function to use when rendering lines. possible values: ("cardinal", "linear", "linear-closed", "step", "step-before", "step-after", "basis", "basis-open", "basis-closed", "bundle", "cardinal-open", "cardinal-closed", "monotone")
#' @export
#' @examples \dontrun{
#' data.frame(year=seq(1790, 1970, 10),
#'            uspop=as.numeric(uspop)) %>%
#'   mjs_plot(x=year, y=uspop) %>%
#'   mjs_line()
#' }
#'
mjs_line <- function(mjs,
                     area=FALSE, animate_on_load=FALSE,
                     interpolate="cardinal") {

  stopifnot(interpolate %in% c("cardinal", "linear", "linear-closed", "step",
                               "step-before", "step-after", "basis", "basis-open",
                               "basis-closed", "bundle", "cardinal-open",
                               "cardinal-closed", "monotone"))


  mjs$x$area <- area
  mjs$x$animate_on_load <- animate_on_load
  mjs$x$geom <- "line"
  mjs$x$interpolate <- interpolate
  mjs
}

#' Add a new line to a metricsgraphics.js linechart "geom"
#'
#' This function adds a line to an existing \code{mjs_line} "geom". Specify
#' the bare name of the column to use in \code{y_accessor} and it will be added
#' to the plot.
#'
#' @note You must have called \code{mjs_line} first before adding additional columns
#' @param mjs plot object
#' @param y_accessor bare name of column to add to the existing line plot
#' @export
#' @examples \dontrun{
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
#' }
mjs_add_line <- function(mjs,
                         y_accessor) {

  stopifnot(y_accessor %in% colnames(mjs))

  multi_line <- mjs$x$multi_line
  if (is.null(multi_line)) multi_line <- list()
  new_line <- as.character(substitute(y_accessor))
  multi_line <- c(multi_line, new_line)
  mjs$x$multi_line <- multi_line
  mjs

}


#' metricsgraphics.js scatterplot "geom"
#'
#' This function adds a point/scatterplot "geom" to a metricsgraphics.js html widget.
#'
#' @param mjs plot object
#' @param point_size the radius of the dots in the scatterplot
#' @param least_squares add a least squares line? (default: \code{FALSE} - no)
#' @param size_accessor bare name of a column to use to scale the size of the points
#' @param color_accessor bare name of a column to use to scale the color of the points
#' @param color_type specifies whether the color scale is quantitative or qualitative. By setting this option to category, you can color the points according to some other discrete value
#' @param size_range specifies the range of point sizes, when point sizes are mapped to data
#' @param x_rug show a "rug" plot next to the x axis? (default: \code{FALSE} - no)
#' @param y_rug show a "rug" plot next to the y axis? (default: \code{FALSE} - no)
#' @param color_range the range of colors, used to color different groups of points.
#' @export
#' @examples \dontrun{
#' mtcars %>%
#'  mjs_plot(x=wt, y=mpg, width=400, height=300) %>%
#'  mjs_point(least_squares=TRUE)
#' }
#'
mjs_point <- function(mjs,
                      point_size=2.5,
                      least_squares=FALSE,
                      size_accessor=NULL,
                      color_accessor=NULL,
                      color_type="number",
                      color_range=c('blue', 'red'),
                      size_range=c(1, 5),
                      x_rug=FALSE,
                      y_rug=FALSE) {

  stopifnot(color_type %in% c("category", "number"))

  mjs$x$chart_type <- "point"
  mjs$x$least_squares<- least_squares
  mjs$x$x_rug <- x_rug
  mjs$x$y_rug <- y_rug
  mjs$x$size_accessor <- substitute(size_accessor)
  mjs$x$color_accessor <- substitute(color_accessor)
  mjs$x$color_type <- color_type
  mjs$x$color_range <- color_range
  mjs$x$size_range <- size_range
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
#' @param show display the axis? (default: \code{TRUE} - yes)
#' @param xax_count tick count
#' @param min_x min limit for x axis
#' @param max_x max limit for x axis
#' @param xax_format how to format tick labels. Currently one of "plain", "comma" or "date"
#' @note xax_format is likely to undergo a drastic change in future releases but support for these three formats will also likely remain.
#' @export
mjs_axis_x <- function(mjs,
                       show=TRUE,
                       xax_count=6,
                       min_x=NULL, max_x=NULL,
                       xax_format="plain") {

  stopifnot(xax_format %in% c("plain", "comma", "date"))

  mjs$x$x_axis <- show
  mjs$x$xax_count <- xax_count
  mjs$x$min_x <- min_x
  mjs$x$max_x <- max_x
  mjs$x$xax_format <- xax_format

  if (xax_format == "date") {
    mjs$x$data[,as.character(mjs$x$x_accessor)] <-
      format(mjs$x$data[,as.character(mjs$x$x_accessor)], "%Y-%m-%d")
  }

  mjs
}

#' Configure y axis ticks & limits
#'
#' @param mjs plot object
#' @param show display the axis? (default: \code{TRUE} - yes)
#' @param yax_count tick count
#' @param min_y min limit for y axis
#' @param max_y max limit for y axis
#' @param y_scale_type scale for y axis; either "linear" (default) or "log"
#' @export
#' @export
mjs_axis_y <- function(mjs,
                       show=TRUE,
                       yax_count=5,
                       min_y=NULL, max_y=NULL,
                       y_scale_type="linear") {

  stopifnot(y_scale_type %in% c("linear", "log"))

  mjs$x$y_axis <- show
  mjs$x$yax_count <- yax_count
  mjs$x$min_y <- min_y
  mjs$x$max_y <- max_y
  mjs$x$y_scale_type <- y_scale_type
  mjs
}


#' Sets a marker line/label
#'
#' metricsgraphics marker lines are vertical lines that identify, say, events or
#' dates worth annotating. This function lets you add a marker to a plot object.
#' you can add as many as you need to.
#'
#' @param mjs plot object
#' @param x_value which x value to draw the marker at
#' @param label text label for the marker
#' @export
#' @examples \dontrun{
#' tmp <- data.frame(year=seq(1790, 1970, 10), uspop=as.numeric(uspop))
#'
#' tmp %>%
#'   mjs_plot(x=year, y=uspop) %>%
#'   mjs_line() %>%
#'   mjs_add_marker(1850, "Something Wonderful") %>%
#'   mjs_add_baseline(150, "Something Awful")
#' }
mjs_add_marker <- function(mjs,
                           x_value, label) {

  if (class(x_value) == "Date") x_value <- format(x_value, "%Y-%m-%d")

  markers <- mjs$x$markers
  if (is.null(markers)) markers <- list()
  new_marker <- list()
  new_marker[[as.character(mjs$x$x_accessor)]] <- x_value
  new_marker[["label"]] <- label
  markers[[length(markers)+1]] <- new_marker
  mjs$x$markers <- markers

  mjs

}

#' Sets a baseline line/label
#'
#' metricsgraphics baselines are horizontal lines that may specify, say, a goal
#' or target to be reached. This function lets you add baselines to a plot object.
#' you can add as many as you need to.
#'
#' @param mjs plot object
#' @param y_value which y value to draw the baseline at
#' @param label text label for the marker
#' @export
#' @examples \dontrun{
#' tmp <- data.frame(year=seq(1790, 1970, 10), uspop=as.numeric(uspop))
#'
#' tmp %>%
#'   mjs_plot(x=year, y=uspop) %>%
#'   mjs_line() %>%
#'   mjs_add_marker(1850, "Something Wonderful") %>%
#'   mjs_add_baseline(150, "Something Awful")
#' }
mjs_add_baseline <- function(mjs,
                             y_value, label) {
  baselines <- mjs$x$baselines
  if (is.null(baselines)) baselines <- list()
  new_baseline <- list()
  new_baseline[["value"]] <- y_value
  new_baseline[["label"]] <- label
  baselines[[length(baselines)+1]] <- new_baseline
  mjs$x$baselines <- baselines
  mjs
}

#' Adds a legend to a metricsgraphics chart
#'
#' @param legend character vector of labels for the legend
#' @export
#' @examples \dontrun{
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
#' }
mjs_add_legend <- function(mjs, legend) {
  mjs$x$legend <- legend
  mjs$x$legend_target <- sprintf("#%s-legend", mjs$elementId)
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

metricsgraphics_html <- function(id, style, class, ...) {
  list(tags$div(id = id, class = class, style=style),
       tags$div(id = sprintf("%s-legend", id), class = sprintf("%s-legend", class)))
}
