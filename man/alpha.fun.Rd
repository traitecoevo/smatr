\name{alpha.fun}
\alias{huber.M}
\alias{alpha.fun}
\title{Utility functions for robust fitting}
\description{
Functions used by \code{\link{sma}} when 'robust = TRUE'. 
}
\usage{
alpha.fun(r, k, q)
huber.M(data, q = pchisq(3, 2))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r,k,q,}{Parameters.}
  \item{data}{.....}
}

\author{ Warton, D. I. \email{David.Warton@unsw.edu.au}, S. Taskinen  }

\seealso{
\code{\link{sma}}
}
\keyword{ internal }
