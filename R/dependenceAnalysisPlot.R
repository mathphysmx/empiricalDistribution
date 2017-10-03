#' @title Bivariate dependence analysis plot
#' @description Plot scatterplots and pseudo-observations plots over a rectangular domain partition by rectangular areas (also called pixes). This function can be used with the function \link{plottingPosition} for the pseudo-observations.
#' @param x table (2-colum matrix or data.frame) of, possibly pseudo-, observations
#' @param xGridBreaks,yGridBreaks zone delimiters. Default to Inter-Quartile Area (iqa := \code{c(0, .25, .5, .75, 1) x c(0, .25, .5, .75, 1)}).
#' @param grid list. grid constructed with (posibly pseudo-) observations of modes, minima,
#' inflexion points or any other points of interest of both variables.
#'  \code{grid} is passed to \link[graphics]{abline}.
#' @param asp The \code{y/x} aspect ratio, see \link[graphics]{plot.window}.
#' @param main Character. Main title for the plot. See \link[graphics]{title}.
#' @param xlab,ylab Character. Label of the axis.
#' @param xlabels,ylabels Character. Labels of the annotations of the axis ticks marks.
#' @param xlSide,ylSide Side where the labels of the axis must be plotted. See \code{side} parameter of \link[graphics]{axis}.
#' @param col color of the points.
#' @param colp color of the pixels.
#' @param ... further parameters to \link[graphics]{points.default}.
#' @export
#' @author Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#' @importFrom "graphics" "axis","box","image.default","par","plot.default","points.default","title"
#' @examples
#' # Example 1: pseudo-observations
#' ne <- 30
#' ue <- matrix(runif(ne), ne/2, 2)
#' xpixelse <- c(0, .25, .5, .75, 1)
#' dependenceAnalysisPlot(x = ue, xGridBreaks = xpixelse) # colp = rainbow(n = npixels),
#'
#' # Example 2: observations
#' ne <- 100
#' ue <- matrix(rnorm(ne), ne/2, 2)
#' xpixelse <- quantile(ue[, 1])
#' ypixelse <- quantile(ue[, 2])
#' dependenceAnalysisPlot(x = ue,  asp = NA,
#'     xlabels = c("min", "q1", "M", "q3", "max"),
#'     xGridBreaks = xpixelse,
#'     yGridBreaks = ypixelse)
#'
#' npixels <- (length(xpixelse) - 1) * (length(ypixelse) - 1)
#' dependenceAnalysisPlot(x = ue,
#'     xGridBreaks = xpixelse,
#'     yGridBreaks = ypixelse,
#'     colp = rainbow(n = npixels),
#'     pch = 20, cex = 2, asp = NA,
#'     grid = list(h = 0, v = c(-1, 1),
#'                 lty = 2, col = "blue"))

# run demo("colors") to see colors available or go to http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# Improvements:
#   Add legend?
#   range(x[, 1], xGridBreaks)
# pixelse <- list(x = 0.5, y = c(0, 0.25, 0.5, 0.75, 1))
# grid = list(modes = list(x = c(0.3, 0.5), y = c(0.23, 0.6, 0.74)),
#               other = list(x = .4, y = c(0.2, 0.8)))
# dependenceAnalysisPlot(x = ue, pixels = pixelse, grid = gride, asp = NA)
# set apart the axes labels from the grid:
#	axis(1, at = atx)

dependenceAnalysisPlot <- function(x = NULL,
                asp = 1, main = "",
                xGridBreaks = c(0, .25, .5, .75, 1),
                yGridBreaks = xGridBreaks,
		xlab="x", ylab="y",
		xlabels=TRUE, ylabels=TRUE,
		xlSide=1, ylSide=2,
                col = par("col"),
                colp = c("white", "gray90"), # run demo("colors") to see colors available or go to http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf
                grid = NULL, ...){

  if(length(colp) == 2){
    sx <- rep_len(x = 0:1, length.out = length(xGridBreaks) - 1)
    sy <- rep_len(x = 0:1, length.out = length(yGridBreaks) - 1)
    fout<- outer(sx, sy, FUN = "+")%%2
  }else{
    sx <- seq_len(length.out = length(xGridBreaks) - 1)
    sy <- seq_len(length.out = length(yGridBreaks) - 1)
    fout <- outer(sx, sy, FUN = "+")
  }

  image.default(x = xGridBreaks,
                y = yGridBreaks,
                z = fout,
                col = colp, asp = asp,
		xlab=xlab, ylab=ylab,
                xaxt = "n", yaxt = "n")
  box()
  axis(xlSide, at = xGridBreaks, labels = xlabels)
  axis(ylSide, at = yGridBreaks, labels = ylabels)
  if(!is.null(grid)){
    linesg <- c(as.list(grid))
    do.call("abline", linesg)
  }
  points.default(x, col = col, ...)
  title(main)

}
