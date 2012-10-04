\name{b.com.est}
\alias{b.com.est}
\title{ Common slope estimation }
\description{
  Estimates a common slope from bivariate data for several independent groups. 
  Called by \code{\link{slope.com}}.
}
\usage{
b.com.est(z, n, method, lambda = 1, res.df)
}

\arguments{
  \item{z}{ Variances and covariances of each group. }
  \item{n}{ Sample sizes of each group. }
  \item{method}{ See \code{\link{slope.com}} for details. }
  \item{lambda}{ Error variance ration (implied by choice of method). }
  \item{res.df}{ Residual degrees of freedom, for each group. }
}
\author{ Warton, D. \email{David.Warton@unsw.edu.au}, translated to R by Ormerod, J. 2005-12-08 }

\seealso{ \code{\link{slope.com}} }
\keyword{ internal }
