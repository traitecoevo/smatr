\name{com.ci}
\alias{com.ci}
\title{ Calculates a confidence inteval for a common slope (called by slope.com) }
\description{
  Calculates a confidence inteval for a common slope (called by slope.com)
}
\usage{
com.ci(b, varb, crit, z, n, l1, l2, method, lambda, res.df)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{b}{ Common slope estimate. }
  \item{varb}{ Estimated variance of the common slope. }
  \item{crit}{ Critical value to be used in confidence interval calculation. }
  \item{z}{ Variances and covariances of each group. }
  \item{n}{ Sample sizes of each group. }
  \item{l1}{ Variance of fitted axis scores, for each group. }
  \item{l2}{ Variance of fitted residuals, for each group. }
  \item{method}{ See slope.com for details. }
  \item{lambda}{ Error variance ration (implied by choice of method. }
  \item{res.df}{ Residual degrees of freedom, for each group. }
}
\author{ Warton, D. \email{David.Warton@unsw.edu.au}, translated to R by Ormerod, J. 2005-12-08 }

\seealso{ \code{\link{slope.com}} }
\keyword{ internal }
