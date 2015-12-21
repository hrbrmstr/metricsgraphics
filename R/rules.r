#' Add a CSS rule to the rendered htmlwidget
#'
#' This function will add a CSS rule to a widget-created
#' DOM stylesheet. \code{rule} should be a valid CSS rule as you
#' would enter in a \code{<style>...</style>} block. No checking is done
#' to ensure validity.
#'
#' Use \code{\{\{ID\}\}} (followed by a space) to target the CSS rule
#' just to the widget vs the whole DOM.
#'
#' Vectorized over \code{rule}
#'
#' @param mjs metricsgraphics plot object
#' @param rule character vector of CSS rule(s) to add to the widget DOM
#' @param warn show warnings for global CSS rules? (default: \code{TRUE})
#' @return metricsgraphics plot object
#' @note This is for expert use only. You need to know quite a bit about the visualization
#'       and target DOM to effectively use this function. CSS rules without the \code{\{\{ID\}\}}
#'       are applied to the entire DOM.
#' @export
#' @examples
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
#'   mjs_add_css_rule("{{ID}} .blk { fill:black }") %>%
#'   mjs_annotate_region("2013-01-01", "2016-01-01", "Crazy times", "blk")
mjs_add_css_rule <- function(mjs, rule, warn=TRUE) {

  # if any of the CSS statements in 'rule' do not have {{ID}} targets, warn the user
  if (warn) {
    if (!any(grepl("\\{\\{ID\\}\\}", rule))) {
      message("NOTE: CSS rules without {{ID}} are applied to the entire DOM.")
    }
  }
  mjs$x$forCSS <- c(mjs$x$forCSS, rule)
  mjs
}
