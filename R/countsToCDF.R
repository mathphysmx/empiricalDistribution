#' @title Computes the empirical distribution function from counts (densities).
#' @description Empirical copula (distribution function) from counts (densities).
#' @param uv 2-column table (matrix or data.frame) of pseudo-observaritions in \code{[0,1]X[0,1]}. Default to NULL. If specified, \code{d} is computed using \link{pj1j2Matrix}.
#' @param d Densities matrix from a bivariate histogram. Maybe a result from the function \link{pj1j2Matrix}.
#' @export
#' @author Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#' @details This function implements the empirical copula in agreement with formula (3) "Carnicero, 2013. Non-parametric copulas ...", and reproduce the same result as the function genmat.copem when k = n, and the Definition 5.6.1 of "Nelsen, 2006. Introduction to copulas"
#' @examples
#' # Example 1
#' library(squash)
#' xye <- cbind(1:5, c(2, 4, 3, 6, 7))
#' uve <- cbind(u = ecdf(xye[,1])(xye[,1]), # Empirical CDF values
#'              v = ecdf(xye[,2])(xye[,2]))
#' Emp_cop_matrix <- EmpCop(uv = uve)
#'
#' # Exapmle 2
#' xye <- cbind(1:5, c(2, 4, 3, 6, 7))
#' uve <- cbind(u = ecdf(xye[,1])(xye[,1]),
#'              v = ecdf(xye[,2])(xye[,2]))
#' ne <- nrow(xye)
#' DensMatrix <- pj1j2Matrix(uv = uve, k = ne)$dens
#' Emp_cop_matrix <- EmpCop(d = DensMatrix)
#'
EmpCop <- function(uv = NULL, d) {

  if(!is.null(uv)){
    n <- nrow(uv)
    d <- pj1j2Matrix(uv = uv, k = n)$dens
  }

  n <- dim(d)[1L] + 1L
  Fmatrix <- matrix(0, n, n)
  Fmatrix[2:n,2:n] <- d
  for(i in 2:n) {
    # i <- 2
    for(j in 2:n) {
      # j <- 2
      Fmatrix[i, j] <- Fmatrix[i-1, j] + sum(d[i-1, 1:(j-1)])
    }
  }

  invisible(Fmatrix)
}
