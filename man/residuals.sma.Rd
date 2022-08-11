\name{residuals.sma}
\alias{residuals.sma}
\title{Extract model residuals
}
\description{Extracts the residuals of a (standardized) major axis fit.
}
\usage{
\method{residuals}{sma}(object, ...)
}
\arguments{
  \item{object}{Object of class 'sma'.}
  \item{\dots}{Further arguments ignored.}
}
\details{
Residuals are calculated as y-bx-a for each group. These values are useful in assumption checking,  especially in constructing residual vs fitted value plots.
}
\value{
A vector of residuals.
}
\seealso{
\code{\link{sma}}, \code{\link{plot.sma}}
}
\keyword{misc}
