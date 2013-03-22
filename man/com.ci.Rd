\name{com.ci}
\alias{com.ci}
\title{ Calculates a confidence inteval for a common slope (called by slope.com) }
\description{
  Calculates a confidence inteval for a common slope (called by slope.com)
}
\usage{
com.ci(b, varb, crit, z, n, l1, l2, method, lambda, res.df, r.factor)
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
  \item{r.factor}{ A vector of "robust factors" for each group, default value 1. Variance estimates are rescaled by this factor when using robust inference. }
}
\author{Warton, D.I.\email{David.Warton@unsw.edu.au}, J. Ormerod, & S. Taskinen}

\seealso{ \code{\link{slope.com}} }
\keyword{ internal }
