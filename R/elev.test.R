#' One-sample test of a (standardised) major axis elevation
#' 
#' @description Test if the elevation of a major axis or standardised major axis equals a
#' specific value. This can now be done via \code{sma(y~x,elev.test=0)}, see
#' help on the \code{\link{sma}} function.
#' 
#' @details Tests if the line relating y to x has an elevation equal to test.value
#' (which has a default value of 0). The line can be a linear regression line,
#' major axis or standardised major axis (as selected using the input argument
#' choice). The test is carried out usinga t-statistic, comparing the
#' difference between estimated and hypothesised elevation to the standard
#' error of elevation. As described in Warton et al (2006).
#' 
#' A confidence interval for the elevation is also returned, again using the
#' t-distribution.
#' 
#' If measurement error is present, it can be corrected for through use of the
#' input argument V, which makes adjustments to the estimated sample variances
#' and covariances then proceeds with the same method of inference. Note,
#' however, that this method is only approximate (see Warton et al 2006 for
#' more details).
#' 
#' The test assumes the following: \enumerate{ \item y and x are linearly
#' related \item residuals independently follow a normal distribution with
#' equal variance at all points along the line }
#' 
#' The assumptions can be visually checked by plotting residual scores against
#' fitted axis scores, and by constructing a Q-Q plot of residuals against a
#' normal distribution, available using the \code{\link{plot.sma}} function.
#' 
#' Setting \code{robust=TRUE} fits lines using Huber's M estimation, and
#' modifies the test statistic as proposed in Taskinen & Warton (in review).
#' 
#' @param y The Y-variable
#' @param x The X-variable
#' @param test.value The hypothesised value of the elevation (default value is
#' 0)
#' @param data Deprecated. Use with() instead (see Examples).
#' @param alpha The desired confidence level for the 100(1-alpha)\% confidence
#' interval for the common slope. (Default value is 0.05, which returns a 95\%
#' confidence interval.)
#' @param method The line fitting method: \describe{ \item{'OLS' or 0}{linear
#' regression} \item{'SMA' or 1}{standardised major axis (this is the default)}
#' \item{'MA' or 2}{major axis} }
#' @param robust If TRUE, uses a robust method to fit the lines and construct
#' the test statistic.
#' @param V The estimated variance matrix of measurement error. Default is that
#' there is no measurement error.
#' @return \item{t}{The test statistic (a t-statistic).} \item{p}{The P-value,
#' taken from the \eqn{t_{n-2}}{t_(n-2)}-distribution. This is an exact test if
#' residuals are normally distributed.} \item{test.value}{The hypothesised
#' value of the elevation.} \item{a}{The estimated elevation.} \item{ci}{A
#' 100(1-alpha)\% CI for the slope.}
#' @author Warton, D.I.\email{David.Warton@@unsw.edu.au}, J. Ormerod, & S.
#' Taskinen
#' @seealso \code{\link{sma}}, \code{\link{line.cis}}, \code{\link{slope.test}}
#' @references Warton D. I., Wright I. J., Falster D. S. and Westoby M. (2006)
#' A review of bivariate line-fitting methods for allometry.  \emph{Biological
#' Reviews} \bold{81}, 259--291.
#' 
#' Taskinen, S. and D.I. Warton. in review. Robust tests for one or more
#' allometric lines.
#' @keywords htest
#' @export
#' @examples
#' 
#' #load the leaflife dataset:
#' data(leaflife)
#' 
#' #consider only the low rainfall sites:
#' leaf.low.rain <-  subset(leaflife, rain=="low")
#' 
#' #construct a plot
#' plot(log10(leaf.low.rain$lma), log10(leaf.low.rain$longev),
#'    xlab="leaf mass per area [log scale]", ylab="leaf longevity [log scale]")
#'     
#' #test if the SMA elevation is 0 for leaf longevity vs LMA
#' with(leaf.low.rain, elev.test(log10(lma), log10(longev)))
#' 
#' #test if the MA elevation is 2
#' with(leaf.low.rain,elev.test(log10(lma), log10(longev),
#'    test.value = 2, method = "MA"))
#' 
elev.test <- function( y, x, test.value=0, data=NULL, alpha=0.05, method="SMA", robust=FALSE, V=matrix(0,2,2) )
{
#     if ( is.null(data)==FALSE )
#     {
#         attach(data)
#     }

  
  if(!is.null(data))
    stop("'data' argument no longer supported.")
  
  
    iref <- ( is.na(x+y) == FALSE ) #to remove NA cases
    n    <- sum(iref)
    res.df <- n - 2
    fcrit  <- qf( 1-alpha, 1, res.df )
    dat    <- cbind( y[iref], x[iref] )
    if ( robust )
    {
		# get robust mean/var matrix:
		q     <- pchisq(3,2)
		S     <- huber.M(dat)
		means <- S$loc
		vr    <- ( S$cov - V) *(n-1)

		# get robust.factors (multipliers on variance matrix):
                rfac  <- robust.factor(dat,q)
 	        r.factor1 <- rfac[1]
                r.factor2 <- rfac[2]
    }
    else
    {
      r.factor1 <- 1
      r.factor2 <- 1 
      means    <- apply(dat,2,mean)
      vr <- ( var(dat) - V )*(n-1) 
    }	
    r      <- vr[1,2]/( ( vr[1,1]*vr[2,2] )^0.5 )

    if ( (method==0) | (method=="OLS") )
    {
        b       <- vr[1,2] / vr[2,2]
        var.res <- ( vr[1,1] - 2*b*vr[1,2] + b^2*vr[2,2] ) / res.df
        var.b   <- var.res / vr[2,2]
    }
    else if ( (method==1) | (method=="SMA") )
    {
        b       <- sign( vr[1,2] )*sqrt( vr[1,1] / vr[2,2] )
        var.res <- ( vr[1,1] - 2*b*vr[1,2] + b^2*vr[2,2] ) / res.df
        var.b   <- ( vr[1,1] - (vr[1,2]^2)/vr[2,2] ) / res.df / vr[2,2]
    }
    else if ( (method==2) | (method=="MA") )
    {
        fac     <- vr[1,1] - vr[2,2]
        b       <- ( fac + sqrt( fac^2 + 4*vr[1,2]^2) ) / 2 / vr[1,2]
        var.res <- ( vr[1,1] - 2*b*vr[1,2] + b^2*vr[2,2] ) / res.df
        var.fit <- ( b^2*vr[1,1] + 2*b*vr[1,2] + vr[2,2] ) / res.df
        var.b   <- 1 / ( var.res/var.fit + var.fit/var.res - 2)*( 1 + b^2 )^2 / res.df    # Use Fisher info
    }

    a        <- means[1] - b*means[2]
    var.a    <- var.res/n*r.factor2 + var.b*means[2]^2*r.factor1
    t        <- (a - test.value)/sqrt(var.a)
    pvalue   <- 2*pt( -abs(t), res.df )

#     if ( is.null(data)==FALSE )
#     {
#         detach(data)
#     }

    list( t=t, a=a, p=pvalue, a.ci=c( a-sqrt(var.a*fcrit), a+sqrt(var.a*fcrit) ), test.value=test.value )
}
