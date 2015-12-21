#' Region annotations for line charts [EXPERIMENTAL]
#'
#' This function uses the \href{https://github.com/senseyeio/mg-regions}{mg-regions}
#' plugin to enable region highlighting with an optional label.
#'
#' This function is also experimental and relies on the plugin maintainer to
#' continue support for it. You should be well-versed in CSS to use this function
#' properly.
#'
#' @param mjs metricsgraphics object
#' @param x_start start point on x axis for region annotation
#' @param x_end end point on x axis for region annotation
#' @param label text label for annotation (leave \code{NULL}) for no label
#' @param css_class CSS class to apply (see References link for more information)
#' @export
#' @return metricsgraphics object
#' @references \url{https://github.com/senseyeio/mg-regions}
#' @examples
#' data.frame(year=seq(1790, 1970, 10),
#'            uspop=as.numeric(uspop)) %>%
#'   mjs_plot(x=year, y=uspop, title="Population Chart") %>%
#'   mjs_line() %>%
#'   mjs_annotate_region(1850, 1900, "Bad stuff") %>%
#'   mjs_annotate_region(1810, 1830, "Stuff")
#'
#' set.seed(1492)
#' stocks <- data.frame(
#'   time = as.Date('2009-01-01') + (365 * 0:9),
#'   X = rnorm(10, 0, 1),
#'   Y = rnorm(10, 0, 2),
#'   Z = rnorm(10, 0, 4))
#'
#' stocks %>%
#'   mjs_plot(x=time, y=X) %>%
#'   mjs_line() %>%
#'   mjs_axis_x(xax_format="date") %>%
#'   mjs_annotate_region("2013-01-01", "2016-01-01", "Crazy times")
#'
#' ## custom region color
#' stocks %>%
#'   mjs_plot(x=time, y=X) %>%
#'   mjs_line() %>%
#'   mjs_axis_x(xax_format="date") %>%
#'   mjs_add_css_rule("{{ID}} .blk { fill:black }") %>%
#'   mjs_annotate_region("2013-01-01", "2016-01-01", "Crazy times", "blk")
mjs_annotate_region <- function(mjs, x_start=NULL, x_end=NULL,
                                label=NULL, css_class=NULL) {

  reg <- list(c(x_start, x_end), label, css_class)
  names(reg) <- c(mjs$x$x_accessor, "label", "class")
  mjs$x$regions[[length(mjs$x$regions)+1]] <- reg
  mjs

}
