% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/line.cis.R
\name{line.cis}
\alias{line.cis}
\title{Slope and elevation of a (standardised) major axis, with confidence
intervals}
\usage{
line.cis(
  y,
  x,
  alpha = 0.05,
  data = NULL,
  method = "SMA",
  intercept = TRUE,
  V = matrix(0, 2, 2),
  f.crit = 0,
  robust = FALSE,
  ...
)
}
\arguments{
\item{y}{The Y-variable}

\item{x}{The X-variable}

\item{alpha}{The desired confidence level for the 100(1-alpha)\% confidence
interval for the common slope. (Default value is 0.05, which returns a 95\%
confidence interval.)}

\item{data}{Deprecated. Use with() instead (see Examples).}

\item{method}{The line fitting method: \describe{ \item{'OLS' or 0}{linear
regression} \item{'SMA' or 1}{standardised major axis (this is the default)}
\item{'MA' or 2}{major axis} }}

\item{intercept}{(logical) Whether or not the line includes an intercept.
\describe{ \item{FALSE}{ no intercept, so the line is forced through the
origin } \item{TRUE}{ an intercept is fitted (this is the default) } }}

\item{V}{The estimated variance matrix of measurement error. Average
measurement error for Y is in the first row and column, and average
measurement error for X is in the second row and column. The default is that
there is no measurement error.}

\item{f.crit}{(optional - rarely required). The critical value to be used
from the F distribution. (Only actually useful for reducing computation time
in simulation work - otherwise, do not change.)}

\item{robust}{If TRUE, uses a robust method to fit the lines and construct
confidence intervals.}

\item{\dots}{Further parameters (not passed anywhere at the moment).}
}
\value{
\item{coeff}{A matrix containing the estimated elevation and slope
(first column), and the lower and upper limits of confidence intervals for
the true elevation and slope (second and third columns). Output for the
elevation and slope are in the first and second rows, respectively.}
}
\description{
Calculates the slope and elevation of a major axis or standardised major
axis, and constructs confidence intervals for the true slope and elevation.
This can now be fitted via calls of the form \code{sma(y~x, ...)}, see
\code{\link{sma}}.
}
\details{
Fits a linear regression line, major axis, or standardised major axis, to a
bivariate dataset. The slope and elevation are returned with confidence
intervals, using any user-specified confidence level.

Confidence intervals are constructed by inverting the standard one-sample
tests for elvation and slope (see slope.test and elev.test for more
details). Only the primary confidence interval is returned - this is valid
as long as it is known a priori that the (standardised) major axis is
estimating the true slope rather than the (standardised) minor axis. For
SMA, this means that the sign of the true slope needs to be known a priori,
and the sample slope must have the same sign as the true slope.

The test assumes the following: \enumerate{ \item y and x are linearly
related \item residuals independently follow a normal distribution with
equal variance at all points along the line }

These assumptions can be visually checked by plotting residuals against
fitted axis scores, and by constructing a Q-Q plot of residuals against a
normal distribution.  An appropriate residual variable is y-bx, and for
fitted axis scores use x (for linear regression), y+bx (for SMA) or by+x
(for MA), where b represents the estimated slope.

If measurement error is present, it can be corrected for through use of the
input argument V, which makes adjustments to the estimated sample variances
and covariances then proceeds with the same method of inference. Note,
however, that this method is only approximate (see Warton et al in review
for more details).

Setting \code{robust=TRUE} fits lines using Huber's M estimation, and
modifies confidence interval formulae along the lines discussed in Taskinen
& Warton (in review).
}
\examples{

#load the leaflife data
data(leaflife)

#consider only the low rainfall sites:
leaf.low.rain=leaflife[leaflife$rain=='low',]

#estimate the SMA line for reserve vs coat
with(leaf.low.rain, line.cis(log10(longev),log10(lma)))

#produce CI's for MA slope and elevation:
with(leaf.low.rain, line.cis(log10(longev),log10(lma),method='MA'))

}
\references{
Warton, D.I., I.J. Wright, D.S. Falster and M. Westoby. 2006.
Bivariate line-fitting methods for allometry. \emph{Biological Reviews}
\bold{81}, 259--291.

Taskinen, S. and D.I. Warton. in review. Robust tests for one or more
allometric lines.
}
\seealso{
\code{\link{sma}}, \code{\link{slope.test}},
\code{\link{elev.test}}
}
\author{
Warton, D.I.\email{David.Warton@unsw.edu.au}, J. Ormerod, & S.
Taskinen
}
\keyword{models}
\keyword{regression}
