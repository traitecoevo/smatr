\name{fitted.sma}
\alias{fitted.sma}
\title{
Returns fitted values
}
\description{
Returns "fitted values" of a (standardized) major axis fit.
}
\usage{
\method{fitted}{sma}(object, type = "fitted", X=NULL, Y=NULL, centered=TRUE, ...)
}
\arguments{
  \item{object}{Object of class \code{sma}.}
  \item{type}{Either 'residuals', or 'fitted'.}
  \item{X,Y}{New X and Y data for which to provide fitted values.}
  \item{centered}{Logical. If TRUE (the default) returns the zero-centered fitted values.}
  \item{\dots}{Further arguments are currently ignored.}
}
\details{
"Fitted values" are calculated using \code{y+bx} for SMA or \code{by+x} for MA (see Warton et al. 2006, especially Table 4). Note these values are calculated differently to ordinary linear regression, and they cannot be interpreted as predicted values. The "fitted values" should be interpreted as measuring how far along the fitted axis each point lies, and are used in checking assumptions (via residual vs. fitted value plots).

Fitted values may be computed for new data, using the \code{X} and \code{Y} arguments. Be aware that \code{X} and \code{Y} need to be of the same length, and it will fail if groups were present in the original model fit. Also, if the model was fit using \code{log='xy'}, for example, make sure to first log10-transform the new data! 
}
\references{ Warton D. I., Wright I. J., Falster D. S. and Westoby M. (2006) A review of bivariate line-fitting methods for allometry.  \emph{Biological Reviews} \bold{81}, 259-291.
}
\seealso{
\code{\link{sma}},\code{\link{residuals.sma}}
}

\keyword{misc}

