#' One-sample test of a (standardised) major axis slope
#' 
#' Test if the slope of a major axis or standardised major axis equals a
#' specific value. This can now be done via \code{sma(y~x, slope.test=1)}, see
#' help on \code{\link{sma}}.
#' 
#' Tests if the line relating y to x has a slope equal to test.value (which has
#' a default value of 1). The line can be a linear regression line, major axis
#' or standardised major axis (as selected using the input argument choice).
#' The test is carried out by testing for correlation between residual and
#' fitted values, as described in Warton et al (in review).
#' 
#' A confidence interval for the slope is also returned, which is the primary
#' confidence interval found by inverting the one-sample test statistic.
#' 
#' If measurement error is present, it can be corrected for through use of the
#' input argument V, which makes adjustments to the estimated sample variances
#' and covariances then proceeds with the same method of inference. Note,
#' however, that this method is only approximate (see Warton et al in review
#' for more details).
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
#' @param test.value The hypothesised value of the slope (default value is 1)
#' @param data Deprecated. Use with() instead (see Examples).
#' @param method The line fitting method: \describe{ \item{'OLS' or 0}{linear
#' regression} \item{'SMA' or 1}{standardised major axis (this is the default)}
#' \item{'MA' or 2}{major axis} }
#' @param alpha The desired confidence level for the 100(1-alpha)\% confidence
#' interval for the common slope. (Default value is 0.05, which returns a 95\%
#' confidence interval.)
#' @param robust If TRUE, uses a robust method to fit the lines and construct
#' the test statistic.
#' @param V The estimated variance matrix of measurement error. Average
#' measurement error for Y is in the first row and column, and average
#' measurement error for X is in the second row and column. The default is that
#' there is no measurement error.
#' @param intercept (logical) Whether or not the line includes an intercept.
#' \describe{ \item{FALSE}{ no intercept, so the line is forced through the
#' origin } \item{TRUE}{ an intercept is fitted (this is the default) } }
#' @return \item{r}{The test statistic - the sample correlation between
#' residuals and fitted values}
#' 
#' \item{p}{The P-value, taken from the F-distribution. This is an exact test
#' if residuals are normally distributed.}
#' 
#' \item{test.value}{The hypothesised value of the slope}
#' 
#' \item{b}{The estimated slope}
#' 
#' \item{ci}{A 100(1-alpha)\% CI for the slope.}
#' @author Warton, D.I.\email{David.Warton@@unsw.edu.au}, J. Ormerod, & S.
#' Taskinen
#' @seealso \code{\link{sma}}, \code{\link{line.cis}}, \code{\link{elev.test}}
#' @references
#' 
#' Warton D. I., Wright I. J., Falster D. S. and Westoby M. (2006) A review of
#' bivariate line-fitting methods for allometry.  \emph{Biological Reviews}
#' \bold{81}, 259--291.
#' 
#' Taskinen, S. and D.I. Warton. in review. Robust tests for one or more
#' allometric lines.
#' @keywords htest
#' @examples
#' 
#' #load the leaflife dataset:
#' data(leaflife)
#' 
#' #consider only the low rainfall sites:
#' leaf.low.rain <- leaflife[leaflife$rain=='low',]
#' 
#' #test if the SMA slope amongst species at low rainfall sites is 1,
#' #for log (base 10) transformed data:
#' with(leaf.low.rain, slope.test(log10(longev), log10(lma)))
#'     
#' #test if the MA slope is 2/3
#' with(leaf.low.rain, slope.test(log10(longev), log10(lma), test.value = 2/3, method = 'MA'))
#' 
slope.test <- function( y, x, test.value=1, data=NULL, method="SMA", alpha=0.05, V=matrix(0,2,2), intercept=TRUE, robust=FALSE )
{

    if ( nargs() < 2 ) 
    {
        stop('Sorry, no can do without two arguments -- Y, X')
    }

#     if ( is.null(data)==FALSE )
#     {
#         attach(data)
#     }

    
    if(!is.null(data))
      stop("'data' argument no longer supported.")
    
    
    iref <- ( is.na(x+y) == FALSE ) #to remove NA cases
    n    <- sum(iref)

    if ( intercept==FALSE )
    {
        resDF <- n - 1 
    }
    else 
    {
        resDF <- n - 2
    }

    fCrit <- qf( 1-alpha, 1, resDF )

    dat <- cbind(y[iref], x[iref])

    if ( robust )
    {
	    if( intercept )
	    {
		# get robust mean/var matrix:
		q     <- pchisq(3,2)
		S     <- huber.M(dat)
		means <- S$loc
		vr    <- ( S$cov - V) *(n-1)

	        r.factor <- robust.factor(dat,q)[1]
	    }
	    else
	    {
		stop("Sorry, robust estimation without an intercept term not yet implemented.")
	    }
    }
    else
    {
          r.factor <- 1
	  if ( intercept )
     	  {
		vr <- ( cov(dat) - V )*(n-1)
    	  }
    	  else
	  {
		vr <- t(dat)%*%dat - V*n
     	  }
    }

    r <- vr[1,2]/sqrt( vr[1,1]*vr[2,2] )

    if(isTRUE(all.equal(r,1))){
      warning("Group found with zero error variance.")
      return(list( F=NA, r=1, p=NA, test.value=test.value, b=NA, ci=c(NA,NA) ))
    }

    bCI     <- matrix( NA, 1, 2 )
    varTest <- matrix( 0, 2, 2 )

    if ( (method==0) | (method=='OLS') )
    {
        b            <- vr[1,2]/vr[2,2]
        varRes       <- ( vr[1,1] - 2*b*vr[1,2] + b^2*vr[2,2] )/resDF
        varB         <- varRes/vr[2,2] * r.factor
        bCI[1,1]     <- b - sqrt(varB)*sqrt(fCrit)
        bCI[1,2]     <- b + sqrt(varB)*sqrt(fCrit)
        varTest[1,1] <- vr[1,1] - 2*test.value*vr[1,2] + test.value^2*vr[2,2]
        varTest[1,2] <- vr[1,2] - test.value*vr[2,2]
        varTest[2,2] <- vr[2,2]
    }
    else if ( (method==1) | (method=='SMA') )
    {
        b            <- sign(vr[1,2])*sqrt(vr[1,1]/vr[2,2])
        B            <- fCrit*( 1 - r^2 )/resDF * r.factor
        bCI[1,1]     <- b*( sqrt(B+1) - sqrt(B) )
        bCI[1,2]     <- b*( sqrt(B+1) + sqrt(B) )
        varTest[1,1] <- vr[1,1] - 2*test.value*vr[1,2] + test.value^2*vr[2,2]
        varTest[1,2] <- vr[1,1] - test.value^2*vr[2,2]
        varTest[2,2] <- vr[1,1] + 2*test.value*vr[1,2] + test.value^2*vr[2,2]
    }
    else if ( (method==2) | (method=='MA') )
    {
        fac          <- vr[1,1] - vr[2,2]
        b            <- ( fac + sqrt( fac^2 + 4*vr[1,2]^2) )/2/vr[1,2]
        Q            <- fCrit*( vr[1,1]*vr[2,2] - vr[1,2]^2 )/resDF * r.factor
        bCI[1,1]     <- ( fac + sqrt( fac^2 + 4*vr[1,2]^2 - 4*Q) )/2/( vr[1,2] + sqrt(Q))
        bCI[1,2]     <- ( fac + sqrt( fac^2 + 4*vr[1,2]^2 - 4*Q) )/2/( vr[1,2] - sqrt(Q))
        if ( ( fac^2 + 4*vr[1,2]^2 - 4*Q) < 0 ) 
        {
            bCI[1,1] <- -Inf
            bCI[1,2] <-  Inf
        }
        varTest[1,1] <- vr[1,1] - 2*test.value*vr[1,2] + test.value^2*vr[2,2]
        varTest[1,2] <- -test.value^2*vr[1,2] + test.value*( vr[1,1] - vr[2,2] ) + vr[1,2]
        varTest[2,2] <- test.value^2*vr[1,1] + 2*test.value*vr[1,2] + vr[2,2]
    }
    else if ( (method==3) | (method=='lamest') )
    {   b=NA
        bCI[1,1:2]   <- NA
    }

     rTest  <- varTest[1,2] / sqrt( varTest[1,1] ) / sqrt( varTest[2,2] )
     F      <- rTest^2/(1 - rTest^2)/r.factor*(n-2)
     pValue <- 1 - pf( F, 1, resDF)

#      if ( is.null(data)==FALSE )
#      {
#         detach(data)
#      }

     list( F=F, r=rTest, p=pValue, test.value=test.value, b=b, ci=bCI )

}
