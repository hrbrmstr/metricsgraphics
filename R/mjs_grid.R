#' Lays out metricsgraphics widgets into a "grid", similar to
#' \code{grid.arrange} from \code{gridExtra}
#'
#' @param ... either individual \code{metricsgraphics} objects or a mixture of
#'     individual \code{metricsgraphics} objects
#' and \code{list}s of \code{metricsgrahics} objects.
#' @param ncol how many columns in the grid
#' @param nrow how many rows in the grid
#' @param widths widths of the cells per row
#' @return \code{htmltools} tag with wrapped \code{metricsgraphics} objects suitable
#'         for knitting with \code{echo=FALSE} & \code{results="asis"} or
#'         rendering in Viewer with \code{html_print}
#' @note \code{mjs_grid} does not work in a Shiny context
#' @importFrom grDevices n2mfrow
#' @export
mjs_grid <- function(..., ncol=1, nrow=1, widths=1) {

  input_list <- as.list(substitute(list(...)))[-1L]

  params <- list()

  for (i in 1:length(input_list)) {
    x <- eval.parent(input_list[[i]])
    if (any(class(x) == "list")) {
      for (j in 1:length(x)) {
        y <- eval(x[[j]])
        params[[length(params)+1]] <- y
      }
    } else {
      params[[length(params)+1]] <- x
    }
  }

  if(!all(sapply(params, function(x) {
    inherits(x, c("metricsgraphics", "htmlwidget"))
  }))) {
    stop("All parameters must be metricsgraphics objects")
  }

  if (ncol == 1 & nrow == 1) {
    nm <- n2mfrow(length(params))
    nrow <- nm[1]
    ncol <- nm[2]
  }

  if (length(widths) < ncol) widths <- rep(1/ncol, ncol)

  if (length(input_list) > ncol*nrow) {
    stop("Number of metricsgraphics objects > available grid slots")
  }

  max_width <- "100%"

  GRID <- TABLE(lapply(seq(1, nrow*ncol, ncol), function(idx){
    TR(width="100%", style="width:100%", lapply(seq(idx, idx+ncol-1, 1), function(cell) {

      cell_val <- try(eval(params[[cell]]), silent=TRUE)
      if (!inherits(cell_val, c("metricsgraphics", "htmlwidget"))) {
        cell_val <- HTML(" &nbsp; ")
      }

      TD(style=sprintf("width:%3.2f%%", 100*widths[cell-idx+1]),
         width=sprintf("%3.2f%%", 100*widths[cell-idx+1]),
         cell_val)
    }))
  }), style=sprintf("width:%s", max_width), width=max_width)

  return(GRID)

}

TABLE <- tags$table <- function(...) tag("table", list(...))
TR <- tags$tr <- function(...) tag("tr", list(...))
TD <- tags$td <- function(...) tag("td", list(...))
