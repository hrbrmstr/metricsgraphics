#' Configure x axis ticks & limits
#'
#' @param mjs metricsgraphics plot object
#' @param show display the axis? (default: \code{TRUE} - yes)
#' @param xax_count tick count
#' @param min_x min limit for x axis
#' @param max_x max limit for x axis
#' @param extended_ticks extend ticks on x axis?
#' @param xax_format how to format tick labels. Currently one of "plain", "comma"
#'        or "date"
#' @param show_secondary_x_label determines whether to show the year, or another
#'        unit of time in the case of smaller series, on the x-axis below
#'        the x-axis labels.
#' @param rug show a "rug" plot next to the x axis? (default: \code{FALSE} - no)
#' @note xax_format is likely to undergo a drastic change in future releases but
#'       support for these three formats will also likely remain.
#' @export
mjs_axis_x <- function(mjs,
                       show=TRUE,
                       xax_count=6,
                       min_x=NULL, max_x=NULL,
                       extended_ticks=FALSE,
                       xax_format="plain",
                       show_secondary_x_label=NULL,
                       rug=FALSE) {

  if (!xax_format %in% c("plain", "comma", "date")) {
    stop("'xax_format' must be either 'plain', 'comma' or 'date'")
  }

  mjs$x$x_axis <- show
  mjs$x$xax_count <- xax_count
  mjs$x$min_x <- min_x
  mjs$x$max_x <- max_x
  mjs$x$x_extended_ticks <- extended_ticks
  mjs$x$show_secondary_x_label <- show_secondary_x_label
  mjs$x$xax_format <- xax_format
  mjs$x$x_rug <- rug

  if (xax_format == "date") {

    if (mjs$x$orig_posix) {

      mjs$x$data[,as.character(mjs$x$x_accessor)] <-
      format(as.POSIXct(mjs$x$data[,as.character(mjs$x$x_accessor)],
                        origin="1970-01-01 00:00:00"),
             "%Y-%m-%dT%H:%M:%SZ")

    } else {
      mjs$x$data[,as.character(mjs$x$x_accessor)] <-
        format(as.Date(mjs$x$data[,as.character(mjs$x$x_accessor)],
                       origin='1970-01-01'), "%Y-%m-%d")
    }
  }

  mjs
}

#' Configure y axis ticks & limits
#'
#' @param mjs metricsgraphics plot object
#' @param show display the axis? (default: \code{TRUE} - yes)
#' @param yax_count tick count
#' @param min_y min limit for y axis
#' @param max_y max limit for y axis
#' @param extended_ticks extend ticks on y axis?
#' @param y_scale_type scale for y axis; either "linear" (default) or "log"
#' @param yax_units a prefix symbol to be shown alongside the y axis' labels.
#'        Useful for currencies, for instance.
#' @param rug show a "rug" plot next to the y axis? (default: \code{FALSE} - no)
#' @return metricsgraphics object
#' @export
mjs_axis_y <- function(mjs,
                       show=TRUE,
                       yax_count=5,
                       min_y=NULL, max_y=NULL,
                       extended_ticks=FALSE,
                       y_scale_type="linear",
                       yax_units="",
                       rug=FALSE) {

  if (!y_scale_type %in% c("linear", "log")) {
    stop("'y_scale_type' must be either 'linear' or 'log'")
  }

  mjs$x$y_axis <- show
  mjs$x$yax_count <- yax_count
  mjs$x$min_y <- min_y
  mjs$x$max_y <- max_y
  mjs$x$y_extended_ticks <- extended_ticks
  mjs$x$y_scale_type <- y_scale_type
  mjs$x$yax_units <- ""
  mjs$x$y_rug <- rug
  mjs
}