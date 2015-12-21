
#' Plot Histograms with MetrisGraphics
#'
#' Given a numeric vector or a data frame and numeric column name (bare or quoted),
#' plot a histogram with the specified parameter. This function automatically a y
#' axis label "Frequency" which you can override with a call to
#' \code{mjs_labs}.
#'
#' @param mjs metricsgraphics plot object
#' @param bar_margin space between bars (defaults to \code{1})
#' @param bins numbenr of bins for the histogram (\code{NULL} == let MetricsGraphcis.js library compute)
#' @return metricsgraphics plot object
#' @export
#' @examples
#' movies <- ggplot2movies::movies[sample(nrow(ggplot2movies::movies), 1000), ]
#'
#' mjs_plot(movies$rating) %>% mjs_histogram()
#'
#' mjs_plot(movies, rating) %>%
#'   mjs_histogram() %>%
#'   mjs_labs(x_label="Histogram of movie ratings")
#'
#' mjs_plot(movies$rating) %>%
#'   mjs_histogram(bins=30)
#'
#' mjs_plot(runif(10000)) %>%
#'   mjs_histogram() %>%
#'   mjs_labs(x_label="runif(10000)")
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
#' @param bins number of bins for the histogram (\code{NULL} == let MetricsGraphcis.js library compute)
#' @param bar_margin space between bars (defaults to \code{1})
#' @return metricsgraphics object
#' @export
#' @examples
#' bimod <- c(rnorm(1000, 0, 1), rnorm(1000, 3, 1))
#'
#' mjs_plot(bimod) %>% mjs_histogram()
#' bimod %>% mjs_hist()
#'
#' mjs_plot(bimod) %>% mjs_histogram(bins=30)
#' bimod %>% mjs_hist(30)
mjs_hist <- function(data, bins=NULL, bar_margin=1) {
  mjs_plot(data) %>%
    mjs_histogram(bins=bins, bar_margin=bar_margin) %>%
    mjs_labs(y_label="Frequency")
}
