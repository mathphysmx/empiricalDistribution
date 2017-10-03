#' @title Plot pseudo-observations
#' @description Scatterplot of pseudo-observations (values in \code{[0,1]}) that preserves the aspect ratio (asp=1).
#' @param bty Bounding box type. See \link[graphics]{par}. A character string which determined the type of box which is drawn about plots. If bty is one of "o" (the default), "l", "7", "c", "u", or "]" the resulting box resembles the corresponding upper case letter. A value of "n" suppresses the box.
#' @param atx,aty The points at which tick-marks are to be drawn. See \code{at} of \link[graphics]{axis}.
#' @inheritParams graphics::plot.default
#' @param ... Arguments passed to \link[graphics]{plot.default}
#' @author Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#' @export
#' @examples
#' uv <- cbind(runif(100), runif(100))
#' plotProbs(uv)
plotProbs <- function(..., bty="o",
		      xlim = 0:1, ylim = 0:1,
		      atx = c(0,0.5, 1),
		      aty = atx){
  plot.default(asp = 1, axes = F, ...)
box(bty = bty)
axis(1, at = atx)
axis(2, at = aty)

}
