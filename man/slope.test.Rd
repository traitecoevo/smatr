\name{slope.test}

\alias{slope.test}

\title{One-sample test of a (standardised) major axis slope}

\description{Test if the slope of a major axis or standardised
 major axis equals a specific value. This can now be done via \code{sma(y~x, slope.test=1)}, see help on \code{\link{sma}}.}

\usage{
    slope.test(y, x, test.value = 1, data=NULL, method = SMA,
        alpha = 0.05, V = matrix(0,2,2), intercept = TRUE, robust=FALSE )

}

\arguments{
  \item{y}{ The Y-variable }
  \item{x}{ The X-variable }
  \item{test.value}{ The hypothesised value of the slope (default value is 1) }
  \item{data}{Deprecated. Use with() instead (see Examples). }
  \item{method}{ The line fitting method:
    \describe{
      \item{'OLS' or 0}{linear regression}
      \item{'SMA' or 1}{standardised major axis (this is the default)}
      \item{'MA' or 2}{major axis}
    }
  }
  \item{alpha}{ The desired confidence level for the 100(1-alpha)\% confidence interval for the common slope. (Default value is 0.05, which returns a 95\% confidence interval.) }
  \item{robust}{If TRUE, uses a robust method to fit the lines and construct the test statistic.}
  \item{V}{ The estimated variance matrix of measurement error. Average measurement error for Y is in the first row and column, and average measurement error for X is in the second row and column. The default is that there is no measurement error. }
  \item{intercept}{ (logical) Whether or not the line includes an  intercept.
    \describe{
      \item{FALSE}{ no intercept, so the line is forced through the origin }
      \item{TRUE}{ an intercept is fitted (this is the default) }
    }
  }
}

\value{
    \item{r}{The test statistic - the sample correlation between residuals and fitted values}

    \item{p}{The P-value, taken from the F-distribution. This is an exact test if residuals are normally distributed.}

    \item{test.value}{The hypothesised value of the slope}

    \item{b}{The estimated slope}

    \item{ci}{A 100(1-alpha)\% CI for the slope.}
}

\details{Tests if the line relating y to x has a slope equal to test.value (which has a default value of 1). The line can be a linear regression line, major axis or standardised major axis (as selected using the input argument choice). The test is carried out by testing for correlation between residual and fitted values, as described in Warton et al (in review).

A confidence interval for the slope is also returned, which is the primary confidence interval found by inverting the one-sample test statistic.

If measurement error is present, it can be corrected for through use of the input argument V, which makes adjustments to the estimated sample variances and covariances then proceeds with the same method of inference. Note, however, that this method is only approximate (see Warton et al in review for more details).

The test assumes the following:
  \enumerate{
    \item y and x are linearly related
    \item residuals independently follow a normal distribution with equal variance at all points along the line
  }      

The assumptions can be visually checked by plotting residual scores against fitted axis scores, and by constructing a Q-Q plot of residuals against a normal distribution, available using the \code{\link{plot.sma}} function.

Setting \code{robust=TRUE} fits lines using Huber's M estimation, and modifies the test statistic as proposed in Taskinen & Warton (in review).

}

\references{ 

Warton D. I., Wright I. J., Falster D. S. and Westoby M. (2006) A review of bivariate line-fitting methods for allometry.  \emph{Biological Reviews} \bold{81}, 259--291.

Taskinen, S. and D.I. Warton. in review. Robust tests for one or more allometric lines.

}

\author{Warton, D.I.\email{David.Warton@unsw.edu.au}, J. Ormerod, & S. Taskinen}

\seealso{\code{\link{sma}}, \code{\link{line.cis}}, \code{\link{elev.test}} }

\examples{
#load the leaflife dataset:
data(leaflife)

#consider only the low rainfall sites:
leaf.low.rain <- leaflife[leaflife$rain=='low',]

#test if the SMA slope amongst species at low rainfall sites is 1,
#for log (base 10) transformed data:
with(leaf.low.rain, slope.test(log10(longev), log10(lma)))
    
#test if the MA slope is 2/3
with(leaf.low.rain, slope.test(log10(longev), log10(lma), test.value = 2/3, method = 'MA'))
}

\keyword{htest}
