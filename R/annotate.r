#' Add a confidence band to line plot
#'
#' If you have lower & upper points associated with your line in a data frame,
#' you can specify their accessors (defaults to \code{"l"} & \code{"u"}) here
#' which will result in a shaded confidence band being plotted with the line.
#'
#' @param mjs metricsgraphics plot object
#' @param lower_accessor bare or quoted name of column to use for the lower
#'        bound of the confidence band
#' @param upper_accessor bare or quoted name of column to use for the upper
#'        boudn of the confidence band
#' @export
#' @examples
#' require(binom)
#' require(dplyr)
#'
#' set.seed(1492)
#' binom.confint(x=sample(2:30, 100, replace=TRUE), n = 100, tol = 1e-8,
#'               methods="bayes") %>%
#'   mutate(x=1:100) -> bdat
#'
#' bdat %>%
#'   mjs_plot(x=x, y=mean, width=600, height=240) %>%
#'   mjs_axis_x(show_secondary_x_label=FALSE,
#'              extended_ticks=TRUE) %>%
#'   mjs_line() %>%
#'   mjs_add_confidence_band(lower_accessor="lower",
#'                           upper_accessor="upper")
mjs_add_confidence_band <- function(mjs, lower_accessor="l", upper_accessor="u") {

  lower_accessor <- substitute(lower_accessor)
  if (inherits(lower_accessor, "name")) {
    lower_accessor <- as.character(lower_accessor)
  }

  upper_accessor <- substitute(upper_accessor)
  if (inherits(upper_accessor, "name")) {
    upper_accessor <- as.character(upper_accessor)
  }

  mjs$x$show_confidence_band = c(lower_accessor, upper_accessor)

  mjs

}

#' Sets a marker line/label
#'
#' metricsgraphics marker lines are vertical lines that identify, say, events or
#' dates worth annotating. This function lets you add a marker to a plot object.
#' you can add as many as you need to.
#'
#' @param mjs metricsgraphics plot object
#' @param x_value which x value to draw the marker at
#' @param label text label for the marker
#' @return metricsgraphics object
#' @export
#' @examples
#' data.frame(
#'   year=seq(1790, 1970, 10),
#'   uspop=as.numeric(uspop)
#' ) %>%
#'   mjs_plot(x=year, y=uspop) %>%
#'   mjs_line() %>%
#'   mjs_add_marker(1850, "Something Wonderful") %>%
#'   mjs_add_baseline(150, "Something Awful")
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
#' @param mjs metricsgraphics plot object
#' @param y_value which y value to draw the baseline at
#' @param label text label for the marker
#' @return metricsgraphics object
#' @export
#' @examples
#' data.frame(
#'   year=seq(1790, 1970, 10),
#'   uspop=as.numeric(uspop)
#' ) %>%
#'   mjs_plot(x=year, y=uspop) %>%
#'   mjs_line() %>%
#'   mjs_add_marker(1850, "Something Wonderful") %>%
#'   mjs_add_baseline(150, "Something Awful")
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