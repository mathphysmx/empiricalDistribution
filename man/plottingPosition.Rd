% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plottingPositions.R
\name{plottingPosition}
\alias{plottingPosition}
\title{Univariate empirical CDF (plotting position)}
\usage{
plottingPosition(xi, a = 0, b = 1)
}
\arguments{
\item{xi}{Numeric vector of observed values.}

\item{a, b}{Numeric scalar. Parameter of the plotting position formula.}
}
\value{
A vector of numerical values of the empirical plotting possitions.
}
\description{
Computes the empirical (observed) distribution of a given variable. These values are also called plotting positions.
}
\details{
This functions implements the general formula (eq. 1.40) for plotting positions in sec. 1.1.4 of Salvadori and de Michelle, 2007, namely:
Fi = (i - a) / (n + 1 - b)
}
\examples{
exi <- rnorm(10)
plottingPosition(xi=exi) # Fn = i/n;   Billingsley, 1995, p. 268
plottingPosition(xi=exi, a = 0, b = 0) # Weibull
plottingPosition(xi=exi, a = 1/3, b = 1/3) # Tukey
}
\references{
Salvadori and de Michelle, 2007. Extremes in Nature: An approach using copulas.
}
\seealso{
the nsRFA R package.
}
