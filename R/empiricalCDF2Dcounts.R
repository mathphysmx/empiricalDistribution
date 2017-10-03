#' @title Computes the empirical bivariate distribution and store the results in a matrix
#' @description Computes the matrix of counts for the empirical distribution.
#' To get the real empirical distribution values divede the result by $n$.
#' This function considers that there are no duplicated values both in $x$ and in $y$.
#' @param x A 2-columns data.frame with numeric observations \eqn{(x1,y1),...,(xn,yn)}.
#' @return An integer \eqn{(n+1)x(n+1)}-matrix with the values of the empirical copula multiplied by $n$.
#' @author Francisco Mendoza-Torres (\email{mentofran@@gmail.com}) and Arturo Erdely (\email{arturo.erdely@comunidad.unam.mx})
#' @references Equation (3) of \href{https://link.springer.com/chapter/10.1007/978-3-642-12465-5_13}{Erdely, A. and Diaz-Viera, M. 2010}.
#' @export
#' @details \code{x[i+1,j+1]} is equivalent to \eqn{Cn(i/n,j/n)}. Deheuvels, 1979 or Nelsen, 2006.
#' @examples
#' # Example 1
#' library(squash)
#' exy <- cbind(1:5, c(2, 4, 3, 6, 7)); print(exy)
#' set.seed(1); exy <- exy[sample(1:5), ]
#' print(empiricalCDF2Dcounts(exy))
#' # to compute the empirical distribution values divide by n
#' (1/5)* empiricalCDF2Dcounts(exy)
#'
#' # Example 2
#' x <- cbind(1:5, c(4, 2, 1, 5, 3))
#' print(empiricalCDF2Dcounts(x))
#' # to compute the empirical distribution values divide by n
#' (1/5)* empiricalCDF2Dcounts(x)
#'
#' # Example 3 (Error catch):
#' exy <- cbind(x=c(1:3,2),y=c(1,2,3,2))
#' # empiricalCDF2Dcounts(exy)
# TODO: Not working with large datasets (~10^5). Implement it in C/C++ or use an alternative algorithm to store the array and/or compute the bernstein polynomial (De'Casteljau algorithm for instance).
empiricalCDF2Dcounts <- function(x){

  # duplicated values catch
  if(any(duplicated(x[,1])) || any(duplicated(x[,2])) || any(duplicated(x[,2])))
    stop('There are duplicated values in x')

  n <- dim(x)[1]
  p <- n + 1
  dist2Dcounts <- matrix(0, ncol = p, nrow = p) # initialize the matrix with zeros. "dist" stands for disrtribution
  xord <- x[order(x[,1]), ] # sort incresingly with respect to the first variable/column

  yrank <- rank(xord[, 2]) + 1
  for(i in 2:p) {
    dist2Dcounts[i, yrank[i-1]:p] <- 1
    dist2Dcounts[i, ] <- dist2Dcounts[i - 1, ] + dist2Dcounts[i, ]
  }

  invisible(dist2Dcounts)
}
