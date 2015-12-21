#' Configure axis labels & plot description
#'
#' @param mjs metricsgraphics object
#' @param x_label label for x asis
#' @param y_label label for y axis
#' @export
#' @return metricsgraphics object
#' @examples
#' mtcars %>%
#'  mjs_plot(x=wt, y=mpg, width=400, height=300) %>%
#'  mjs_point(color_accessor=carb, size_accessor=carb) %>%
#'  mjs_labs(x="Weight of Car", y="Miles per Gallon")
#'
mjs_labs <- function(mjs,
                     x_label=NULL, y_label=NULL) {
  mjs$x$x_label <- x_label
  mjs$x$y_label <- y_label
  mjs
}
