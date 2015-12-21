#' metricsgraphics.js scatterplot "geom"
#'
#' This function adds a point/scatterplot "geom" to a metricsgraphics.js html widget.
#'
#' @param mjs metricsgraphics plot object
#' @param point_size the radius of the dots in the scatterplot
#' @param least_squares add a least squares line? (default: \code{FALSE} - no)
#' @param size_accessor bare or quoted name of a column to use to scale the size of the points
#' @param color_accessor bare or quoted name of a column to use to scale the color of the points
#' @param color_type specifies whether the color scale is quantitative or qualitative.
#'        By setting this option to "\code{category}", you can color the points according to some
#'        other discrete value
#' @param size_range specifies the range of point sizes, when point sizes are mapped to data
#' @param x_rug show a "rug" plot next to the x axis? (default: \code{FALSE} - no)
#' @param y_rug show a "rug" plot next to the y axis? (default: \code{FALSE} - no)
#' @param color_range the range of colors, used to color different groups of points.
#' @return metricsgraphics object
#' @export
#' @examples
#' mtcars %>%
#'  mjs_plot(x=wt, y=mpg, width=400, height=300) %>%
#'  mjs_point(least_squares=TRUE)
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