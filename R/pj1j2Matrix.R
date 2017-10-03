#' @title Empirical counts/density computing
#'
#' @description Computes formula (5) of \href{https://link.springer.com/article/10.1007/s00477-013-0733-y}{Carnicero et al., 2013}.
#'
#' @param uv 2-columns data.frame of nonexedance probabilities.
#' @param plot Plot pixels form of 2D histogram? Default to FALSE
#' @param k A positive integer. the order of the Bernstein polynomial.
#'     Default to cubic root of the number of observations as recommended in page 1998, Carnicero, 2013.
#' @param ... Other arguments passed to \code{hist2D}.
#' @return A 2D matrix whose elements (pj1pj2) are indexed the same way as pj1j2 in formula 5 of Carnicero.
#' @export
#' @references
#' \href{https://link.springer.com/article/10.1007/s00477-013-0733-y}{Carnicero et al., 2013}
#' @author Francisco Mendoza-Torres (\email{mentofran@@gmail.com})
#' @details Here comes the details.
#' @examples
#' ne <- 30
#' set.seed(12)
#' uve <- cbind(runif(ne), runif(ne))
#' pj1j2Matrix(uv = uve, k = 3, plot = TRUE)
# # k is best when is a factor. see \link{factorize}
pj1j2Matrix <- function(uv, k = NULL, plot = FALSE, ...) {

  n <- nrow(uv)
  if (is.null(k))
    k <- n # ceiling(n^(1/3)) # Carnicero recomendation on page 1998
  breaks <- (0:k) / k
  Allpj1j2_counts <- hist2D(x = uv,
                            xbreaks = breaks, ybreaks = breaks,
                            plot = plot, ...)$z
  Allpj1j2_counts[is.na(Allpj1j2_counts)] <- 0
  Allpj1j2 <- Allpj1j2_counts / n
  # points(uv, col = "lightblue") sum(Allpj1j2, na.rm  = T) # check that sums to
  # 1 p43 <- Allpj1j2[4,3] # This shows that the subindexes of "p" correspond to
  # the matrix elements
  return(list(counts = Allpj1j2_counts, dens = Allpj1j2))
}
