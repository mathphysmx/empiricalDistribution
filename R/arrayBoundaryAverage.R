#' @title Averages the boundary elements of an array
#' @description This function was implemented in order to reproduce the average of page 1998 of \href{https://link.springer.com/article/10.1007/s00477-013-0733-y}{Carnicero et al., 2013}.
#' @param pj1pj2Matrix Empirical copula matrix. It is assumed that the periodic variable is the first column of the argument \code{uv} in the function \code{\link{pj1j2Matrix}}.
#' @param var2 Unimplemented yet. Which is the periodic random variable? Default to the first column of the argument \code{uv} in the function \code{\link{pj1j2Matrix}}.
#' @return A numeric array with equall (averaged) boundary elements.
#' @export
#' @author Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#' @details This function was implemented in order to reproduce the average of page 1998 of \href{https://link.springer.com/article/10.1007/s00477-013-0733-y}{Carnicero et al., 2013}. By default the periodic variable is the one of the first colum of the argument \code{uv} of the function pj1j2Matrix.
#' @examples
#' ne <- 30
#' set.seed(12)
#' uve <- cbind(runif(ne), runif(ne))
# source('R/pj1j2Matrix.R');
#' library(squash)
#' arrayBoundaryAverage(pj1j2Matrix(uv = uve, k = 3)$counts)
# previouslly named pj1j2MatrixTilde.R
arrayBoundaryAverage <- function(pj1pj2Matrix, var2 = FALSE) {

  k <- dim(pj1pj2Matrix)[1]

#   if(var2) {
#     Allpj1j2_tilde <- t(pj1pj2Matrix)
#   } else {
    Allpj1j2_tilde <- pj1pj2Matrix
#   }

  Allpj1j2_tilde[1, ] <- (pj1pj2Matrix[1,] + pj1pj2Matrix[k,])/2
  Allpj1j2_tilde[k, ] <- (pj1pj2Matrix[1,] + pj1pj2Matrix[k,])/2

#   if(var2) {
#     Allpj1j2_tilde <- t(Allpj1j2_tilde)
#   }

  return(Allpj1j2_tilde)
}
