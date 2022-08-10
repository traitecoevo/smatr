#' Slope and elevation of a (standardised) major axis, with confidence
#' intervals
#' 
#' @description Calculates the slope and elevation of a major axis or standardised major
#' axis, and constructs confidence intervals for the true slope and elevation.
#' This can now be fitted via calls of the form \code{sma(y~x, ...)}, see
#' \code{\link{sma}}.
#' 
#' @details Fits a linear regression line, major axis, or standardised major axis, to a
#' bivariate dataset. The slope and elevation are returned with confidence
#' intervals, using any user-specified confidence level.
#' 
#' Confidence intervals are constructed by inverting the standard one-sample
#' tests for elvation and slope (see slope.test and elev.test for more
#' details). Only the primary confidence interval is returned - this is valid
#' as long as it is known a priori that the (standardised) major axis is
#' estimating the true slope rather than the (standardised) minor axis. For
#' SMA, this means that the sign of the true slope needs to be known a priori,
#' and the sample slope must have the same sign as the true slope.
#' 
#' The test assumes the following: \enumerate{ \item y and x are linearly
#' related \item residuals independently follow a normal distribution with
#' equal variance at all points along the line }
#' 
#' These assumptions can be visually checked by plotting residuals against
#' fitted axis scores, and by constructing a Q-Q plot of residuals against a
#' normal distribution.  An appropriate residual variable is y-bx, and for
#' fitted axis scores use x (for linear regression), y+bx (for SMA) or by+x
#' (for MA), where b represents the estimated slope.
#' 
#' If measurement error is present, it can be corrected for through use of the
#' input argument V, which makes adjustments to the estimated sample variances
#' and covariances then proceeds with the same method of inference. Note,
#' however, that this method is only approximate (see Warton et al in review
#' for more details).
#' 
#' Setting \code{robust=TRUE} fits lines using Huber's M estimation, and
#' modifies confidence interval formulae along the lines discussed in Taskinen
#' & Warton (in review).
#' 
#' @param y The Y-variable
#' @param x The X-variable
#' @param alpha The desired confidence level for the 100(1-alpha)\% confidence
#' interval for the common slope. (Default value is 0.05, which returns a 95\%
#' confidence interval.)
#' @param data Deprecated. Use with() instead (see Examples).
#' @param method The line fitting method: \describe{ \item{'OLS' or 0}{linear
#' regression} \item{'SMA' or 1}{standardised major axis (this is the default)}
#' \item{'MA' or 2}{major axis} }
#' @param V The estimated variance matrix of measurement error. Average
#' measurement error for Y is in the first row and column, and average
#' measurement error for X is in the second row and column. The default is that
#' there is no measurement error.
#' @param intercept (logical) Whether or not the line includes an intercept.
#' \describe{ \item{FALSE}{ no intercept, so the line is forced through the
#' origin } \item{TRUE}{ an intercept is fitted (this is the default) } }
#' @param f.crit (optional - rarely required). The critical value to be used
#' from the F distribution. (Only actually useful for reducing computation time
#' in simulation work - otherwise, do not change.)
#' @param robust If TRUE, uses a robust method to fit the lines and construct
#' confidence intervals.
#' @param \dots Further parameters (not passed anywhere at the moment).
#' @return \item{coeff}{A matrix containing the estimated elevation and slope
#' (first column), and the lower and upper limits of confidence intervals for
#' the true elevation and slope (second and third columns). Output for the
#' elevation and slope are in the first and second rows, respectively.}
#' @author Warton, D.I.\email{David.Warton@@unsw.edu.au}, J. Ormerod, & S.
#' Taskinen
#' @seealso \code{\link{sma}}, \code{\link{slope.test}},
#' \code{\link{elev.test}}
#' @references Warton, D.I., I.J. Wright, D.S. Falster and M. Westoby. 2006.
#' Bivariate line-fitting methods for allometry. \emph{Biological Reviews}
#' \bold{81}, 259--291.
#' 
#' Taskinen, S. and D.I. Warton. in review. Robust tests for one or more
#' allometric lines.
#' @keywords models regression
#' @examples
#' 
#' #load the leaflife data
#' data(leaflife)
#' 
#' #consider only the low rainfall sites:
#' leaf.low.rain=leaflife[leaflife$rain=='low',]
#' 
#' #estimate the SMA line for reserve vs coat
#' with(leaf.low.rain, line.cis(log10(longev),log10(lma)))
#' 
#' #produce CI's for MA slope and elevation:
#' with(leaf.low.rain, line.cis(log10(longev),log10(lma),method='MA'))
#' 
line.cis <- function( y, x, alpha=0.05, data=NULL, method="SMA", intercept=TRUE, V=matrix(0,2,2), f.crit=0, robust=FALSE,...)
{

    # instead of attaching
    #if (!is.null(data))attach(data) 
	
  
  if(!is.null(data))
    stop("'data' argument no longer supported.")
  
  
    dat  <- data.frame( y, x )
    datm <- as.matrix(na.omit(dat))
    n <- nrow(datm)
    
    # was:
    # # Remove NA cases
    # iref <- !is.na(x+y) 
    # n    <- sum(iref)
    #datm <- as.matrix( dat[iref,] )
    # Removed 'iref' everywhere below, since datm already has no missing values.
    
    # if the line is forced through the origin, df are n-1 not n-2
    if ( intercept )
        res.df <- n-2
    else
        res.df <- n-1 

    if (f.crit == 0)f.crit <- qf( 1 - alpha, 1, res.df )

    #if the line is forced through the origin, SS are estimated without centring the data.
    if ( robust )
    {
	    if( intercept )
	    {
		# get robust mean/var matrix:
		q     <- pchisq(3,2)
		S     <- huber.M(datm)
		means <- S$loc
		vr    <- ( S$cov - V) *(n-1)

		# get robust factors (multiplier on variance matrix):

                rfac  <- robust.factor(datm,q)
 	        r.factor1 <- rfac[1]
                r.factor2 <- rfac[2]
	    }
	    else
	    {
		stop("Sorry, robust estimation without an intercept term not yet implemented.")
	    }
    }
    else
    {
        r.factor1 <- 1
        r.factor2 <- 1 
	if ( intercept )
	{
    	    vr <- ( var(datm) - V )*(n-1) 
 	    means    <- apply(datm,2,mean)
	}
    	else 
    	    vr <- t(datm) %*% datm - V*n
    }	

	
    r   <- vr[1,2] / sqrt( vr[1,1]*vr[2,2] )
    cis <- matrix( 0, 2, 2)

    
    if ( method == 0 | method=="OLS" )
    {
        lab      <- "coef(reg)"
        b        <- vr[1,2] / vr[2,2]
        var.res  <- ( vr[1,1] - 2*b*vr[1,2] + b^2*vr[2,2] ) / res.df
        var.b    <- var.res / vr[2,2] * r.factor1
        cis[2,1] <- b - sqrt(var.b)*sqrt(f.crit)
        cis[2,2] <- b + sqrt(var.b)*sqrt(f.crit)
    }
    if ( method==1 | method=="SMA" )
    {
        lab      <- "coef(SMA)"
        b        <- sign( vr[1,2] ) * sqrt( vr[1,1] / vr[2,2] )
        bigb     <- f.crit * ( 1 - r^2 ) / res.df * r.factor1
        cis[2,1] <- b*( sqrt(bigb+1) - sqrt(bigb) )
        cis[2,2] <- b*( sqrt(bigb+1) + sqrt(bigb) )
	  if(b<0) #to ensure the lower limit is the more negative limit
		cis[2,] = cis[2,c(2,1)]
        var.res  <- ( vr[1,1] - 2*b*vr[1,2] + b^2*vr[2,2] ) / res.df
        var.b    <- ( vr[1,1] - vr[1,2]^2/vr[2,2] ) / res.df/vr[2,2] * r.factor1
    }
    if ( method==2 | method=="MA" )
    {
	lab      <- "coef(MA)"
        fac      <- vr[1,1] - vr[2,2]
        b        <- ( fac + sqrt( fac^2 + 4*vr[1,2]^2 ) ) / 2 / vr[1,2]
        Q        <- f.crit*( vr[1,1]*vr[2,2] - vr[1,2]^2 ) / res.df * r.factor1
        if ( (fac^2 + 4*vr[1,2]^2 - 4*Q ) < 0 )
        {
            cis[2,1] <- -Inf
            cis[2,2] <-  Inf
        }
	  else
	  {
            cis[2,1] <- (fac + sqrt( fac^2 + 4*vr[1,2]^2 - 4*Q ) ) / 2 / ( vr[1,2] + sqrt(Q) )
            cis[2,2] <- (fac + sqrt( fac^2 + 4*vr[1,2]^2 - 4*Q ) ) / 2 / ( vr[1,2] - sqrt(Q) )
	  	if ( Q>vr[1,2]^2 & fac>0 ) #MA limits overlap Y-axis
	  	{
			warning(paste("Note this CI includes the Y-axis - the actual CI is (",cis[2,1],",infinity) and (-infinity,", cis[2,2], ")"))
		}
	  }
        var.res  <- ( vr[1,1] - 2*b*vr[1,2] + b^2*vr[2,2] ) / res.df
        var.fit  <- ( b^2*vr[1,1] + 2*b*vr[1,2] + vr[2,2] ) / res.df
        var.b    <- 1 / ( var.res/var.fit + var.fit/var.res - 2 )*( 1 + b^2 )^2 / res.df * r.factor1
    }

    if (intercept)
    {
        a        <- means[1] - b*means[2]
        var.a  <- var.res/n*r.factor2 + var.b*means[2]^2
        cis[1,1] <- a - sqrt(var.a)*sqrt(f.crit)
        cis[1,2] <- a + sqrt(var.a)*sqrt(f.crit)
    }
    else
    {
        a        <- 0
        cis[1,]  <- NA
    }

    coeff           <- rbind( a, b )
    coef.names      <- c( "elevation", "slope" )
    coeff           <- data.frame( coeff, cis )
    names(coeff)    <- c( lab, "lower limit", "upper limit" )
    rownames(coeff) <- coef.names

	#if (!is.null(data))detach(data)
	
    return(coeff)
}
