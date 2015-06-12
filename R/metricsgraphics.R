#' Create a new metricsgraphics.js  plot
#'
#' \code{mjs_plot()} initializes the metricsgraphics.js html widget
#' and takes a data frame & (bare or quoted) x & y column names as minimum input.
#' This must be piped to a "geom" (metricsgraphics.js only supports single
#' "geom" layers) and can also be piped to other \code{mjs_} functions that
#' manipulate aesthetics.
#'
#' See \href{http://metricsgraphicsjs.org/}{MetricsGraphics.js} for more information.
#'
#' @param data data frame
#' @param x bare or quoted name of column to use for x values
#' @param y bare or quoted name of column to use for y values
#' @param show_rollover_text determines whether or not to show any text when a data point is rolled over.
#' @param linked inks together all other graphs whose linked option is set to true.
#'        When one graphic in that set is rolled over, the corresponding values in the other
#'        graphics are also rolled over (default: \code{FALSE} - not linked)
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
#' @return metricsgraphics object
#' @export
#' @examples \dontrun{
#' data.frame(year=seq(1790, 1970, 10),
#'            uspop=as.numeric(uspop)) %>%
#'   mjs_plot(x=year, y=uspop) %>%
#'   mjs_line()
#'
#' # accessor params can also be quoted
#'
#' data.frame(year=seq(1790, 1970, 10),
#'            uspop=as.numeric(uspop)) %>%
#'   mjs_plot(x="year", y="uspop") %>%
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

  if (!format %in% c("percentage", "count")) {
    stop("'format' must be either 'percentage' or 'count'")
  }

  eid <- sprintf("mjs-%s",
                 paste(sample(c(letters[1:6], 0:9), 30, replace=TRUE), collapse=""))

  if (!missing(x)) {
    x <- substitute(x)
    if (inherits(x, "name")) { x <- as.character(x) }
  } else {
    x <- as.character(substitute(x))
  }

  if (!missing(y)) {
    y <- substitute(y)
    if (inherits(y, "name")) { y <- as.character(y) }
  } else {
    y <- as.character(substitute(y))
  }

  is_datetime <- function(x) {
    inherits(x, c('Date', 'POSIXct', 'POSIXlt'))
  }
  if (is.null(dim(data))) {
    if (is_datetime(data)) data <- as.numeric(data)
  } else if (is_datetime(data[, x])) {
    data[, x] <- as.numeric(data[, x])
  }

  params = list(
    data=data,
    x_axis=TRUE,
    y_axis=TRUE,
    baseline_accessor=NULL,
    predictor_accessor=NULL,
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
    min_x=NULL,
    max_x=NULL,
    min_y=NULL,
    max_y=NULL,
    bar_margin=1,
    binned=FALSE,
    bins=NULL,
    least_squares=FALSE,
    interpolate="cardinal",
    decimals=decimals,
    show_rollover_text=show_rollover_text,
    x_accessor=x,
    y_accessor=y,
    multi_line=NULL,
    geom="line",
    legend=NULL,
    legend_target=NULL,
    y_extended_ticks=FALSE,
    x_extended_ticks=FALSE,
    target=sprintf("#%s", eid)
  )

  if (is.null(width)) params$full_width <- TRUE
  if (is.null(height)) params$full_height <- TRUE

  htmlwidgets::createWidget(
    name = 'metricsgraphics',
    x = params,
    width = width,
    height = height,
    package = 'metricsgraphics',
    elementId = eid
  )

}

#' Plot Histograms with MetrisGraphics
#'
#' Given a numeric vector or a data frame and numeric column name (bare or quoted),
#' plot a histogram with the specified parameter. This function automatically a y
#' axis label "Frequency" which you can override with a call to
#' \code{mjs_labs}.
#'
#' @param mjs plot object
#' @param bar_margin space between bars (defaults to \code{1})
#' @param bins numbenr of bins for the histogram (\code{NULL} == let MetricsGraphcis.js library compute)
#' @return metricsgraphics plot object
#' @export
#' @examples \dontrun{
#' movies <- movies[sample(nrow(movies), 1000), ]
#' mjs_plot(movies$rating) %>% mjs_histogram()
#' mjs_plot(movies, rating) %>% mjs_histogram() %>% mjs_labs(x_label="Histogram of movie ratings")
#' mjs_plot(movies$rating) %>% mjs_histogram(bins=30)
#' mjs_plot(runif(10000)) %>% mjs_histogram() %>% mjs_labs(x_label="runif(10000)")
#' }
mjs_histogram <- function(mjs, bar_margin=1, bins=NULL) {

  mjs$x$chart_type <- "histogram"
  mjs$x$bar_margin <- bar_margin
  mjs$x$binned <- FALSE
  mjs$x$bins <- bins
  mjs$x$y_label <- "Frequency"
  mjs$x$geom <- "hist"

  x <- as.character(mjs$x$x_accessor)

  if (mjs$x$binned == FALSE) {
    if (x != "") {
      mjs$x$data <- as.numeric(mjs$x$data[,x])
    } else {
      mjs$x$data <- as.numeric(mjs$x$data)
    }
  }

  mjs

}

#' Shortcut for plotting MetricsGraphics histograms
#'
#' This function performs the call to \code{mjs_plot} and assumes
#' \code{data} is a numeric vector. It's intended to save keystrokes
#' when plotting quick histograms. This function automatically a y
#' axis label "Frequency" which you can override with a call to
#' \code{mjs_labs}.
#'
#' @param data numeric vector
#' @param bins numbenr of bins for the histogram (\code{NULL} == let MetricsGraphcis.js library compute)
#' @param bar_margin space between bars (defaults to \code{1})
#' @return metricsgraphics object
#' @export
#' @examples \dontrun{
#' bimod <- c(rnorm(1000, 0, 1), rnorm(1000, 3, 1))
#'
#' mjs_plot(bimod) %>% mjs_histogram()
#' bimod %>% mjs_hist()
#'
#' mjs_plot(bimod) %>% mjs_histogram(bins=30)
#' bimod %>% mjs_hist(30)
#' }
mjs_hist <- function(data, bins=NULL, bar_margin=1) {
  mjs_plot(data) %>%
    mjs_histogram(bins=bins, bar_margin=bar_margin) %>%
    mjs_labs(y_label="Frequency")
}

#' metricsgraphics.js bar chart "geom"
#'
#' This function adds a bar "geom" to a metricsgraphics.js html widget.
#'
#' @param mjs plot object
#' @param bar_height width of bars
#' @param binned is data already binned? (default: \code{TRUE} - yes)
#' @return metricsgraphics object
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
  mjs$x$data[, mjs$x$y_accessor] <- as.character(mjs$x$data[, mjs$x$y_accessor])
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
#' @param interpolate the interpolation function to use when rendering lines.
#'        possible values: ("cardinal", "linear", "linear-closed", "step", "step-before",
#'        "step-after", "basis", "basis-open", "basis-closed", "bundle", "cardinal-open",
#'        "cardinal-closed", "monotone")
#' @return metricsgraphics object
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

  if(!interpolate %in% c("cardinal", "linear", "linear-closed", "step",
                               "step-before", "step-after", "basis", "basis-open",
                               "basis-closed", "bundle", "cardinal-open",
                               "cardinal-closed", "monotone")) {
    stop("'interpolate' must be a valid value")
  }


  mjs$x$area <- area
  mjs$x$animate_on_load <- animate_on_load
  mjs$x$geom <- "line"
  mjs$x$interpolate <- interpolate
  mjs
}

#' Add a new line to a metricsgraphics.js linechart "geom"
#'
#' This function adds a line to an existing \code{mjs_line} "geom". Specify
#' the bare or quoted name of the column to use in \code{y_accessor} and it will be added
#' to the plot.
#'
#' @note You must have called \code{mjs_line} first before adding additional columns
#' @param mjs plot object
#' @param y_accessor bare or quoted name of column to add to the existing line plot
#' @return metricsgraphics object
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

  y_accessor <- substitute(y_accessor)
  if (inherits(y_accessor, "name")) { y_accessor <- as.character(y_accessor) }

  multi_line <- mjs$x$multi_line
  if (is.null(multi_line)) multi_line <- list()
  new_line <- y_accessor
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
#' @param size_accessor bare or quoted name of a column to use to scale the size of the points
#' @param color_accessor bare or quoted name of a column to use to scale the color of the points
#' @param color_type specifies whether the color scale is quantitative or qualitative.
#'        By setting this option to category, you can color the points according to some other discrete value
#' @param size_range specifies the range of point sizes, when point sizes are mapped to data
#' @param x_rug show a "rug" plot next to the x axis? (default: \code{FALSE} - no)
#' @param y_rug show a "rug" plot next to the y axis? (default: \code{FALSE} - no)
#' @param color_range the range of colors, used to color different groups of points.
#' @return metricsgraphics object
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

  if (!color_type %in% c("category", "number")) {
    stop("'color_type' must be either 'category' or 'number'")
  }

  mjs$x$chart_type <- "point"
  mjs$x$least_squares<- least_squares
  mjs$x$x_rug <- x_rug
  mjs$x$y_rug <- y_rug
  if (class(substitute(size_accessor)) != "NULL") {
    size_accessor <- substitute(size_accessor)
    if (inherits(size_accessor, "name")) { size_accessor <- as.character(size_accessor) }
    mjs$x$size_accessor <- size_accessor
  }
  if (class(substitute(color_accessor)) != "NULL") {
    color_accessor <- substitute(color_accessor)
    if (inherits(color_accessor, "name")) { color_accessor <- as.character(color_accessor) }
    mjs$x$color_accessor <- color_accessor
  }
  mjs$x$color_type <- color_type
  mjs$x$color_range <- color_range
  mjs$x$size_range <- size_range
  mjs$x$geom <- "point"
  mjs
}

#' Configure axis labels & plot description
#'
#' @param mjs metricsgraphics object
#' @param x_label label for x asis
#' @param y_label label for y axis
#' @export
#' @return metricsgraphics object
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
#' @param extended_ticks extend ticks on x axis?
#' @param xax_format how to format tick labels. Currently one of "plain", "comma" or "date"
#' @note xax_format is likely to undergo a drastic change in future releases but
#'       support for these three formats will also likely remain.
#' @export
mjs_axis_x <- function(mjs,
                       show=TRUE,
                       xax_count=6,
                       min_x=NULL, max_x=NULL,
                       extended_ticks=FALSE,
                       xax_format="plain") {

  if (!xax_format %in% c("plain", "comma", "date")) {
    stop("'xax_format' must be either 'plain', 'comma' or 'date'")
  }

  mjs$x$x_axis <- show
  mjs$x$xax_count <- xax_count
  mjs$x$min_x <- min_x
  mjs$x$max_x <- max_x
  mjs$x$x_extended_ticks <- extended_ticks
  mjs$x$xax_format <- xax_format

  if (xax_format == "date") {
    mjs$x$data[,as.character(mjs$x$x_accessor)] <-
      format(as.Date(mjs$x$data[,as.character(mjs$x$x_accessor)],origin='1970-01-01'), "%Y-%m-%d")
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
#' @param extended_ticks extend ticks on y axis?
#' @param y_scale_type scale for y axis; either "linear" (default) or "log"
#' @return metricsgraphics object
#' @export
mjs_axis_y <- function(mjs,
                       show=TRUE,
                       yax_count=5,
                       min_y=NULL, max_y=NULL,
                       extended_ticks=FALSE,
                       y_scale_type="linear") {

  if (!y_scale_type %in% c("linear", "log")) {
    stop("'y_scale_type' must be either 'linear' or 'log'")
  }

  mjs$x$y_axis <- show
  mjs$x$yax_count <- yax_count
  mjs$x$min_y <- min_y
  mjs$x$max_y <- max_y
  mjs$x$y_extended_ticks <- extended_ticks
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
#' @return metricsgraphics object
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
#' @return metricsgraphics object
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
#' @param mjs plot object
#' @param legend character vector of labels for the legend
#' @export
#' @return metricsgraphics object
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

#' @title Adds a custom rollover to a metricsgraphics chart
#' @details MetricsGraphics charts allow for \href{https://github.com/mozilla/metrics-graphics/wiki/Graphic#mouseover}{custom rollovers}.
#'     \code{mjs_add_mouseover} lets you add a custom rollover to a metricsgraphics object. You must be
#'     familiar with javascript and D3 idioms since you are supplying a javascript function as
#'     a parameter.\cr
#'     \cr
#'     Since targeting is done by element id, you will need to add a special string - \code{\{\{ID\}\}} -
#'     to the target element selector so metricsgraphics can add the unique object identifier
#'     to the selector. See Examples for basic usage.
#' @param mjs plot object
#' @param func text for javascript function to be used for the custom rollover. See Details for usage.
#' @export
#' @return metricsgraphics object
#' @examples \dontrun{
#' dat %>%
#'   mjs_plot(x=date, y=value) %>%
#'   mjs_line() %>%
#'   mjs_axis_x(xax_format = "date") %>%
#'   mjs_add_mouseover("function(d, i) {
#'                 $('{{ID}} svg .mg-active-datapoint')
#'                     .text('custom text : ' + d.date + ' ' + i);
#'                  }")
#' }
#' @export
mjs_add_mouseover <- function(mjs, func) {
  mjs$x$mouseover <- JS(gsub("\\{\\{ID\\}\\}", sprintf("%s", mjs$x$target), func))
  mjs
}

#' Widget output function for use in Shiny
#'
#' @param outputId output id
#' @param width width
#' @param height height
#' @export
metricsgraphicsOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'metricsgraphics', width, height, package = 'metricsgraphics')
}

#' Widget render function for use in Shiny
#'
#' @param expr expr
#' @param env env
#' @param quoted quoted
#' @export
renderMetricsgraphics <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, metricsgraphicsOutput, env, quoted = TRUE)
}

metricsgraphics_html <- function(id, style, class, ...) {
  list(tags$div(id = id, class = class, style = style),
       tags$div(id = sprintf("%s-legend", id), class = sprintf("%s-legend", class)))
}
