#' @title Matrix of empirical copula.
#' @description Computes the matrix of empirical copula.
#' @param mat.xy A 2-columns data.frame with the observations \eqn{(x1,y1),...,(xn,yn)}.
#' @return A \eqn{(n+1)x(n+1)}- matrix with the values of the empirical copula.
#' @examples
#' x <- cbind(1:5, c(2, 4, 3, 6, 7))
#' genmat.copem(x)
#' @details \code{mat.copem[i+1,j+1]} is equivalent to \eqn{Cn(i/n,j/n)}. Deheuvels, 1979 or Nelsen, 2006
#' @references Equation (3) of \href{https://link.springer.com/chapter/10.1007/978-3-642-12465-5_13}{Erdely, A. and Diaz-Viera, M. 2010}.
#' @export
genmat.copem <- function(mat.xy){
  # ----------------------------------------
  # MATRIZ DE VALORES DE LA COPULA EMPIRICA
  # ----------------------------------------
  #
  # Input: { (x1,y1),...,(xn,yn) }
  #
  # Output: matriz de (n+1)x(n+1) con valores de la copula empirica
  #
  # mat.copem[i+1,j+1] equivalente a Cn(i/n,j/n)
  #
  n <- dim(mat.xy)[1]
  mat.copem <- matrix(0, ncol = (n + 1), nrow = (n + 1))
  mat.xyord <- mat.xy
  orden <- order(mat.xy[, 1])
  for(i in 1:n) {
    mat.xyord[i, ] <- mat.xy[orden[i], ]
  }
  mat.copem[n + 1, ] <- (0:n)/n
  y.ord <- sort(mat.xyord[, 2])
  for(i in 1:(n - 1)) {
    columna <- (((mat.xyord[, 2][i] <= y.ord)) * 1)/n
    mat.copem[i + 1, ] <- mat.copem[i, ] + c(0, columna)
  }
  return(mat.copem)
}
