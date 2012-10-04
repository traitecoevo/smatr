\name{fitted.sma}
\alias{fitted.sma}
\title{
Returns fitted values
}
\description{
Returns "fitted values" of a (standardized) major axis fit.
}
\usage{
\method{fitted}{sma}(object, ..., type = "fitted")
}
\arguments{
  \item{object}{Object of class 'sma'.}
  \item{type}{Either 'residuals', or 'fitted'.}
  \item{\dots}{Further arguments - ignored.}
}
\details{
"Fitted values" (Warton et al 2006) are calculated using y+bx-a for SMA or by+x-a for MA. Note these values are calculated differently to ordinary linear regression, and they cannot be interpreted as predicted values. The "fitted values" should be interpreted as measuring how far along the fitted axis each point lies, and are used in checking assumptions (via residual vs fitted value plots).
}
\references{ Warton D. I., Wright I. J., Falster D. S. and Westoby M. (2006) A review of bivariate line-fitting methods for allometry.  \emph{Biological Reviews} \bold{81}, 259--291.
}
\seealso{
\code{\link{sma}},\code{\link{residuals.sma}}
}

\keyword{misc}

