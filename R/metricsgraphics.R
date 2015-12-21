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
#' @param format sets the format of the data object, which is to say,
#'        \code{count}s or \code{percentage}s
#' @param missing_is_hidden if true and if the data object is a time series, missing data points will be treated as zeros
#' @param left the size of the left margin in pixels.
#' @param right the size of the right margin in pixels.
#' @param top the size of the top margin in pixels.
#' @param bottom the size of the bottom margin in pixels.
#' @param buffer the buffer size in pixels between the actual chart area and the margins.
#' @param width Width in pixels (optional, defaults to automatic sizing)
#' @param height Height in pixels (optional, defaults to automatic sizing)
#' @param title plot title
#' @param description plot description
#' @note Plot \code{title} and \code{description} work best when the widget is in
#'       a Bootstrap template. They also increase the overall plot area (height,
#'       mostly) since they add \code{<div>}s. The \code{description} will be
#'       visible in the upper left area (on \code{?} hover) if not displayed
#'       in a Boostrap template.
#' @return metricsgraphics object
#' @export
#' @examples
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
#'
mjs_plot <- function(data, x, y,
                     show_rollover_text = TRUE,
                     linked = FALSE,
                     decimals=2, format="count",
                     missing_is_hidden=FALSE,
                     left = 80, right = 10,
                     top = 40, bottom = 60, buffer = 8,
                     width = NULL, height = NULL,
                     title=NULL, description=NULL) {

  if (!format %in% c("percentage", "count")) {
    stop("'format' must be either 'percentage' or 'count'")
  }

  if (inherits(data, "data.frame")) data <- data.frame(data, stringsAsFactors=FALSE)

  eid <- sprintf("mjs-%s",
                 paste(sample(c(letters[1:6], 0:9), 30, replace=TRUE), collapse=""))

  if (!missing(x)) {
    x <- substitute(x)
    res <- try(eval(x, data, parent.frame()), silent = TRUE)
    if (!inherits(res, "try-error") && inherits(res, "character")) {
      if (length(res) != 1) {
        x <- as.character(substitute(x))
      } else {
        x <- res
      }
    } else if (inherits(x, "name")) { x <- as.character(x) }
  } else {
    x <- as.character(substitute(x))
  }

  if (!missing(y)) {
    y <- substitute(y)
    res <- try(eval(y, data, parent.frame()), silent = TRUE)
    if (!inherits(res, "try-error") && inherits(res, "character")) {
      if (length(res) != 1) {
        y <- as.character(substitute(y))
      } else {
        y <- res
      }
    } else if (inherits(y, "name")) { y <- as.character(y) }
  } else {
    y <- as.character(substitute(y))
  }

  is_datetime <- function(x) {
    inherits(x, c('Date', 'POSIXct', 'POSIXlt'))
  }

  is_posix <- function(x) {
    inherits(x, c('POSIXct', 'POSIXlt'))
  }

  orig_posix <- FALSE
  if (is.null(dim(data))) {
    if (is_posix(data)) orig_posix <- TRUE
  } else if (is_posix(data[, x])) {
    orig_posix <- TRUE
  }

  if (is.null(dim(data))) {
    if (is_datetime(data)) data <- as.numeric(data)
  } else if (is_datetime(data[, x])) {
    data[, x] <- as.numeric(data[, x])
  }

  params = list(
    forCSS=NULL,
    regions=NULL,
    orig_posix=orig_posix,
    data=data,
    x_axis=TRUE,
    y_axis=TRUE,
    baseline_accessor=NULL,
    predictor_accessor=NULL,
    show_confidence_band=NULL,
    show_secondary_x_label=NULL,
    chart_type="line",
    xax_format="plain",
    x_label=NULL,
    y_label=NULL,
    markers=NULL,
    baselines=NULL,
    linked=linked,
    title=title,
    description=description,
    left=left,
    right=right,
    bottom=bottom,
    buffer=buffer,
    format=format,
    y_scale_type="linear",
    yax_count=5,
    xax_count=6,
    x_rug=FALSE,
    y_rug=FALSE,
    area=FALSE,
    missing_is_hidden=missing_is_hidden,
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
    yax_units="",
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

#' Widget output function for use in Shiny
#'
#' @param outputId output id
#' @param width width
#' @param height height
#' @export
metricsgraphicsOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'metricsgraphics', width, height, package = 'metricsgraphics')
}

#' Widget render function for use in Shiny
#'
#' @param expr expr
#' @param env env
#' @param quoted quoted
#' @export
renderMetricsgraphics <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, metricsgraphicsOutput, env, quoted = TRUE)
}

metricsgraphics_html <- function(id, style, class, ...) {
  list(tags$div(id = id, class = class, style = style),
       tags$div(id = sprintf("%s-legend", id), class = sprintf("%s-legend", class)))
}
