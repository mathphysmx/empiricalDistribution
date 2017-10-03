#' @title Univariate empirical CDF (plotting position)
#' @description Computes the empirical (observed) distribution of a given variable. These values are also called plotting positions.
#' @param xi Numeric vector of observed values.
#' @param a,b Numeric scalar. Parameter of the plotting position formula.
#'
#' @return A vector of numerical values of the empirical plotting possitions.
#' @export
#' @references Salvadori and de Michelle, 2007. Extremes in Nature: An approach using copulas.
#' @seealso the nsRFA R package.
#' @details This functions implements the general formula (eq. 1.40) for plotting positions in sec. 1.1.4 of Salvadori and de Michelle, 2007, namely:
#' Fi = (i - a) / (n + 1 - b)
#' @examples
#' exi <- rnorm(10)
#' plottingPosition(xi=exi) # Fn = i/n;   Billingsley, 1995, p. 268
#' plottingPosition(xi=exi, a = 0, b = 0) # Weibull
#' plottingPosition(xi=exi, a = 1/3, b = 1/3) # Tukey
#  TODO: add for the 2D case (plottingPosition2D.R) <12-09-17, yourname> #
# TODO: add directional empirical distribution (page 100 of Mardia, 2000. Directional statistics)
plottingPosition <- function (xi, a = 0, b = 1) {

	# os <- order(xi) # order statistics
	n <- length(xi)
	i <- 1:n
	fi <- (i - a) / (n + 1 - b)
	return(fi)
}
