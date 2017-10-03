#' @title First order forward difference of a matrix
#previouslly called # previously named deltaEC.R
#' @description First order forward difference of a matrix.
#' This function was originally developed for the empirical copula distribution in matrix form.
#' @param mat An \code{m x n} matrix. Maybe a result from the function \link{empiricalCDF2Dcounts}.
#' @param i First(X) or second variable(Y)? by row (i=1) or by columns (i=2)?
#' @return if \code{i=1}, an \code{(m - 1) x n} matrix
#' if \code{i=2}, an \code{m x (n - 1)} matrix.
#' @author Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#' @export
#' @examples
#' set.seed(123)
#' xe <- matrix(rpois(12, 3), nrow = 4, ncol = 3)
#' print(xe)
#' efdx  <- forwardDifference(xe,   i = 1); print(efdx)
#' efdy  <- forwardDifference(xe,   i = 2); print(efdy)
#' efdxy <- forwardDifference(efdx, i = 2); print(efdxy) # mixed forward difference
#' forwardDifference(xe^2)/forwardDifference(xe) # finite differences of the function y=x^2
forwardDifference <- function(mat, i = 1){

  if(i == 1)
    dEC <-  diff(   mat)
  if(i == 2)
    dEC <- t(diff(t(mat)))

  invisible(dEC)
}
