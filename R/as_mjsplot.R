#' Turn a simple ggplot plot into an mjs_plot object
#'
#' Takes a ggplot object that has a single geom (it can be geom_line,
#' geom_point or geom_histogram) and converts it to it's metricsgraphics counterpart.
#' It will do it's best to identify plot labels and, in the case of
#' geom_histogram, the number of bins.
#'
#' @param gg ggplot object
#' @return metricsgraphics object
#' @export
#' @examples
#' dat <- data.frame(year=seq(1790, 1970, 10),
#'                   uspop=as.numeric(uspop))
#'
#' set.seed(5689)
#' movies <- movies[sample(nrow(movies), 1000), ]
#'
#' gg1 <- ggplot(dat, aes(x=year, y=uspop)) + geom_line()
#' as_mjsplot(gg1)
#'
#' gg2 <- ggplot(dat, aes(x=year, y=uspop)) + geom_point()
#' as_mjsplot(gg2)
#'
#' gg3 <- ggplot(movies, aes(rating)) + geom_histogram()
#' as_mjsplot(gg3)
#'
#' gg4 <- ggplot(movies, aes(rating)) + geom_histogram(binwidth = 0.1)
#' as_mjsplot(gg4)
as_mjsplot <- function(gg) {

  if (!inherits(gg, c("gg", "ggplot"))) {
    stop("as_mjsplot only works with ggplot objects", call.=FALSE)
  }

  gb <- ggplot_build(gg)

  if (length(gb$plot$layers) > 1) {
    stop("as_mjsplot only works with single-layer-geoms", call.=FALSE)
  }

  plot_type <- gb$plot$layers[[1]]$geom$objname

  if (plot_type=="line") {
    mjs_labs( mjs_line(mjs_plot(gb$data[[1]], x="x", y="y")),
              x_label=gb$plot$labels$x,
              y_label=gb$plot$labels$y)
  } else if (plot_type=="point") {
    mjs_labs(mjs_point(mjs_plot(gb$data[[1]], x="x", y="y")),
             x_label=gb$plot$labels$x,
             y_label=gb$plot$labels$y)
  } else if (plot_type=="histogram") {
    x <- as.character(gg3$mapping$x)
    mjs_labs(mjs_histogram(mjs_plot_(gb$plot$data, x=x),
                           bins=nrow(gb$data[[1]])),
             x_label=gb$plot$labels$x,
             y_label=gb$plot$labels$y)
  } else {
    stop("as_mjsplot only works with geom_line, geom_point and geom_histogram", call.=FALSE)
  }

}

#' Create a new metricsgraphics.js  plot
#'
#' \code{mjs_plot()} initializes the metricsgraphics.js html widget
#' and takes a data frame & variables holding character strings for
#' the  x & y column names as minimum input.
#' This must be used with or piped to a "geom" (metricsgraphics.js only supports single
#' "geom" layers) and can also be piped to other \code{mjs_} functions that
#' manipulate aesthetics.
#'
#' See \href{http://metricsgraphicsjs.org/}{MetricsGraphics.js} for more information.
#'
#' @param data data frame
#' @param x variable holding character name for data.frame column
#' @param y varaible holding character name for data.frame column
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
#' @return metricsgraphics object
#' @export
mjs_plot_ <- function (data, x, y, show_rollover_text = TRUE, linked = FALSE,
          decimals = 2, format = "count", missing_is_hidden = FALSE,
          left = 80, right = 10, top = 40, bottom = 60, buffer = 8,
          width = NULL, height = NULL)
{
  if (!format %in% c("percentage", "count")) {
    stop("'format' must be either 'percentage' or 'count'")
  }
  eid <- sprintf("mjs-%s", paste(sample(c(letters[1:6], 0:9),
                                        30, replace = TRUE), collapse = ""))

  x <- as.character(x)

  if (missing(y)) {
    y <- as.character(substitute(y))
  }

  is_datetime <- function(x) {
    inherits(x, c("Date", "POSIXct", "POSIXlt"))
  }
  is_posix <- function(x) {
    inherits(x, c("POSIXct", "POSIXlt"))
  }
  orig_posix <- FALSE
  if (is.null(dim(data))) {
    if (is_posix(data))
      orig_posix <- TRUE
  }
  else if (is_posix(data[, x])) {
    orig_posix <- TRUE
  }
  if (is.null(dim(data))) {
    if (is_datetime(data))
      data <- as.numeric(data)
  }
  else if (is_datetime(data[, x])) {
    data[, x] <- as.numeric(data[, x])
  }
  params = list(orig_posix = orig_posix, data = data, x_axis = TRUE,
                y_axis = TRUE, baseline_accessor = NULL, predictor_accessor = NULL,
                show_confidence_band = NULL, show_secondary_x_label = NULL,
                chart_type = "line", xax_format = "plain", x_label = NULL,
                y_label = NULL, markers = NULL, baselines = NULL, linked = linked,
                title = NULL, description = NULL, left = left, right = right,
                bottom = bottom, buffer = buffer, format = format, y_scale_type = "linear",
                yax_count = 5, xax_count = 6, x_rug = FALSE, y_rug = FALSE,
                area = FALSE, missing_is_hidden = missing_is_hidden,
                size_accessor = NULL, color_accessor = NULL, color_type = "number",
                color_range = c("blue", "red"), size_range = c(1, 5),
                bar_height = 20, min_x = NULL, max_x = NULL, min_y = NULL,
                max_y = NULL, bar_margin = 1, binned = FALSE, bins = NULL,
                least_squares = FALSE, interpolate = "cardinal", decimals = decimals,
                show_rollover_text = show_rollover_text, x_accessor = x,
                y_accessor = y, multi_line = NULL, geom = "line", yax_units = "",
                legend = NULL, legend_target = NULL, y_extended_ticks = FALSE,
                x_extended_ticks = FALSE, target = sprintf("#%s", eid))
  if (is.null(width))
    params$full_width <- TRUE
  if (is.null(height))
    params$full_height <- TRUE
  htmlwidgets::createWidget(name = "metricsgraphics", x = params,
                            width = width, height = height, package = "metricsgraphics",
                            elementId = eid)
}