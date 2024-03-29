% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/residuals.sma.R
\name{residuals.sma}
\alias{residuals.sma}
\title{Extract model residuals}
\usage{
\method{residuals}{sma}(object, ...)
}
\arguments{
\item{object}{Object of class 'sma'.}

\item{\dots}{Further arguments ignored.}
}
\value{
A vector of residuals.
}
\description{
Extracts the residuals of a (standardized) major axis fit.
}
\details{
Residuals are calculated as y-bx-a for each group. These values are useful
in assumption checking, especially in constructing residual vs fitted value
plots.
}
\seealso{
\code{\link{sma}}, \code{\link{plot.sma}}
}
\keyword{misc}
