#' @title 2D histogram.
#' @description Creates a 2D version of a histogram.
#' This function is used by the function "pj1pj2Matrix" found in the file pj1pj2 for directinal data.
#' @param y same as x
#' @param ny same as nx
#' @param ylim same as xlim
#' @param ybreaks same as xbreaks
#' @param ylab same as ylab
#' @param breaks see \link[graphics]{hist}
#' @param right see \link[graphics]{hist}
#' @inheritParams squash::hist2
#' @importFrom "grDevices" "xy.coords"
#' @importFrom "squash" "matapply","hist2","colorgram","heat","prettyInt"
#' @author Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#' @export
#' @examples
#' ne <- 20
#' set.seed(12)
#' uve <- cbind(runif(ne), runif(ne))
#' print(hist2D(uve, nx = 3))
hist2D <- function (x, y = NULL, nx = 50, ny = nx, xlim = NULL, ylim = NULL,
                    xbreaks = NULL, ybreaks = NULL, plot = TRUE, xlab = NULL,
                    ylab = NULL, zlab = "Counts", colFn = heat,
                    breaks = prettyInt,
                    right = TRUE, ...)
{
  xy <- xy.coords(x, y)
  firstNonNull <- function(...) unlist(list(...))[1]
  xlab <- firstNonNull(xlab, xy$xlab, deparse(substitute(x)),
                       "x")
  ylab <- firstNonNull(ylab, xy$ylab, deparse(substitute(y)),
                       "y")
  z <- rep(1, length(xy$x))
  h <- squash::matapply(xy$x, xy$y, z, FUN = length, nx = nx, ny = ny,
                xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks,
                right = right)
  h$xlab <- xlab
  h$ylab <- ylab
  h$zlab <- zlab
  if (plot) {
    squash::colorgram(h, colFn = colFn, breaks = breaks, ...)
  }
  invisible(h)
}
