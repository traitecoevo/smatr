\name{coef.sma}
\alias{coef.sma}
\title{Extract coefficients from a 'sma' or 'ma' fit}
\description{Extracts elevation and slope of a standardized major axis (sma) or major axis (ma) fit,
for each of the groups if the fit was by group.}
\usage{
\method{coef}{sma}(object, ...)
}
\arguments{
  \item{object}{Object of class 'sma'.}
  \item{\dots}{Further arguments ignored.}
}
\value{
A dataframe with the slope(s) and elevation(s), and their confidence intervals. If the fit was by multiple groups,
fits by all groups are returned.
}
\author{
R.A. Duursma
}
\seealso{\code{\link{sma}}
}

\keyword{misc}
