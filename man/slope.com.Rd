\name{slope.com}
\alias{slope.com}
\title{ Common slope test amongst several allometric lines }
\description{
  Test if several major axis or standardised major axis lines share a common slope. This can now be done via \code{sma(y~x*groups)}, see help on the \code{\link{sma}} function.
}
\usage{
slope.com(y, x, groups, method = 'SMA', alpha = 0.05, 
   data = NULL, intercept = TRUE, robust=FALSE,
   V = array(0, c(2, 2, length(unique(groups)))), 
   group.names = sort(unique(groups)),
   ci = TRUE, bs = TRUE, slope.test=NULL)
}
\arguments{
  \item{y}{ The Y-variable for all observations (as a vector) }
  \item{x}{ The X-variable for all observations (as a vector) }
  \item{groups}{ Coding variable identifying which group each observation belongs to (as a factor or vector) }
  \item{method}{ The line fitting method:
    \describe{
      \item{'SMA' or 1}{standardised major axis (this is the default)}
      \item{'MA' or 2}{major axis}
      \item{'lamest' or 3}{Error variance ratio is estimated from the data}
    }
  }
  \item{alpha}{ The desired confidence level for the 100(1-alpha)\% confidence interval for the common slope. (Default value is 0.05, which returns a 95\% confidence interval.) }
  \item{data}{ Deprecated. Use with() instead (see Examples).}
  \item{intercept}{ (logical) Whether or not the line includes an  intercept.
    \describe{
      \item{FALSE}{ no intercept, so the line is forced through the origin }
      \item{TRUE}{ an intercept is fitted (this is the default) }
    }
  }
  \item{robust}{If TRUE, uses a robust method to fit the lines and construct the test statistic.}
  \item{V}{ The estimated variance matrices of measurement error, for each group. This is a 3-dimensional array with measurement error in Y in the first row and column, error in X in the second row and column, and groups running along the third dimension. Default is that there is no measurement error. }
  \item{group.names}{ (optional: rarely required). A vector containing the labels for `groups'. (Only actually useful for reducing computation time in simulation work). }
  \item{ci}{ (logical) Whether or not to return a confidence interval for the common slope. }
  \item{bs}{ (logical) Whether or not to return the slopes for the separate groups, with confidence intervals. }
  \item{slope.test}{If a value provided, tests the common slope fit against this value.}
}

\value{
  \item{lr }{The (Bartlett-corrected) likelihood ratio statistic testing for common slope}
  \item{p }{The P-value of the test. This is calculated assuming that lr has a chi-square distribution with (g-1) df, if there are g groups}
  \item{b }{The common slope estimate}
  \item{varb }{The sample variance of the common slope}
  \item{ci }{A 100(1-alpha)\% confidence interval for the common slope}
  \item{lambda }{The error variance ratio - the ratio of error variance in y to error variance in x. For MA, this is assumed to be 1. for SMA, this is assumed to be \eqn{b^2}{b^2}. For the `lamest' method, the error variance ratio is estimated from the data under the common slope assumption.}
  \item{bs }{The slopes and confidence intervals for data from each group.}
}

\details{
For several bivariate groups of observations, this function tests if the line-of-best-fit has a common slope for all samples, when the line-of-best-fit is estimated using the major axis, standardised major axis, or a more general version of these methods in which the error variance ratio is estimated from the data.

The test assumes the following:
\enumerate{
  \item each group of observations was independently sampled
  \item y and x are linearly related within each group
  \item residuals independently follow a normal distribution with equal variance at all points along the line, within each group
  }
Note that we do not need to assume equal variance across groups, unlike in the standard test for common slope for linear regression.

The assumptions can be visually checked by plotting residual scores against fitted axis scores, and by constructing a Q-Q plot of residuals against a normal distribution, available using the \code{\link{plot.sma}} function.

Setting \code{robust=TRUE} fits lines using Huber's M estimation, and modifies the test statistic as proposed in Taskinen & Warton (in review).

The common slope is estimated from a maximum of 100 iterations, convergence is reached when the change in b is \eqn{< 10^{-6}}{< 10^-6}.
}
\references{ Warton D. I. and Weber N. C. (2002) Common slope tests for bivariate structural relationships.  \emph{Biometrical Journal} \bold{44}, 161--174.

Warton D. I., Wright I. J., Falster D. S. and Westoby M. (2006) A review of bivariate line-fitting methods for allometry.  \emph{Biological Reviews} \bold{81}, 259--291.

Taskinen, S. and D.I. Warton. in review. Robust tests for one or more allometric lines.

}
\author{Warton, D.I.\email{David.Warton@unsw.edu.au}, J. Ormerod, & S. Taskinen}

\seealso{ \code{\link{sma}}, \code{\link{line.cis}}, \code{\link{elev.com}}, \code{\link{shift.com}} }

\examples{
#load leaf longevity data
data(leaflife)

#plot the data, with different symbols for different groups.
plot(leaflife$lma, leaflife$longev, type='n', log='xy', xlab=
   'leaf mass per area [log scale]', ylab='leaf longevity [log scale]')
colours <- c('blue', 'red', 'green', 'yellow')
points(leaflife$lma, leaflife$longev,
   col=colours[as.numeric(leaflife$site)])
legend(55, 5, as.character(unique(leaflife$site)), col=colours,
   pch=rep(1,4))

#test for common SMA slope of log(leaf longevity) vs log(LMA),
#across species sampled at different sites:
fit <- with(leaflife, slope.com(log10(longev), log10(lma), site))
fit

#Residual vs fits plots for SMA fit of each site
y <- log10(leaflife$longev)
x <- log10(leaflife$lma)
site <- leaflife$site
par( mfrow=c(2,2) )
plot(y[site==1] + fit$bs[1,1] * x[site==1], y[site==1] - fit$bs[1,1] 
   * x[site==1], xlab='fits (site 1)', ylab='residuals (site 1)')
plot(y[site==2] + fit$bs[1,2] * x[site==2], y[site==2] - fit$bs[1,2]
   * x[site==2], xlab='fits (site 2)', ylab='residuals (site 2)')
plot(y[site==3] + fit$bs[1,3] * x[site==3], y[site==3] - fit$bs[1,3]
   * x[site==3], xlab='fits (site 3)', ylab='residuals (site 3)')
plot(y[site==4] + fit$bs[1,4] * x[site==4], y[site==4] - fit$bs[1,4]
   * x[site==4], xlab='fits (site 4)', ylab='residuals (site 4)')

#Test for common SMA slope amongst species at low rainfall sites
#with different levels of soil nutrients
leaf.low.rain <- leaflife[leaflife$rain=='low',]
with(leaf.low.rain, slope.com(log10(longev), log10(lma), soilp))

#test for common MA slope:
with(leaflife, slope.com(log10(longev), log10(lma), site, method='MA'))

#test for common MA slope, and produce a 90% CI for the common slope:
with(leaflife, slope.com(log10(longev), log10(lma), site,  method='MA', alpha=0.1))

}
\keyword{ htest }
