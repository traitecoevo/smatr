#' Test for no mean shift along a common (standardised) major axis
#' 
#' Test if several groups of observations have no shift in location along major
#' axis or standardised major axis lines with a common slope. This can now be
#' done via \code{sma(y~x+groups, type="shift")}, see help on the
#' \code{\link{sma}} function.
#' 
#' Calculates a Wald statistic to test for no shift along several MA's or SMA's
#' of common slope. This is done by testing for equal fitted axis means across
#' groups.
#' 
#' Note that this test is only valid if it is reasonable to assume that the
#' axes for the different groups all have the same slope.
#' 
#' The test assumes the following: \enumerate{ \item each group of observations
#' was independently sampled \item the axes fitted to all groups have a common
#' slope \item y and x are linearly related within each group \item fitted axis
#' scores independently follow a normal distribution with equal variance at all
#' points along the line, within each group } Note that we do not need to
#' assume equal variance across groups, unlike in tests comparing several
#' linear regression lines.
#' 
#' The assumptions can be visually checked by plotting residuals against fitted
#' axis scores, and by constructing a Q-Q plot of residuals against a normal
#' distribution, available using
#' 
#' \code{plot.sma(sma.object,which="residual")}.
#' 
#' On a residual plot, if there is a distinct increasing or decreasing trend
#' within any of the groups, this suggests that all groups do not share a
#' common slope.
#' 
#' A plot of residual scores against fitted axis scores can also be used as a
#' visual test for no shift. If fitted axis scores systematically differ across
#' groups then this is evidence of a shift along the common axis.
#' 
#' Setting \code{robust=TRUE} fits lines using Huber's M estimation, and
#' modifies the test statistic as proposed in Taskinen & Warton (in review).
#' 
#' The common slope (\eqn{\hat{\beta}}{b}) is estimated from a maximum of 100
#' iterations, convergence is reached when the change in \eqn{\hat{\beta}}{b}
#' is \eqn{< 10^{-6}}{< 10^-6}.
#' 
#' @param y The Y-variable for all observations (as a vector)
#' @param x The X-variable for all observations (as a vector)
#' @param groups Coding variable identifying which group each observation
#' belongs to (as a factor or vector)
#' @param data Deprecated. Use with() instead (see Examples).
#' @param method The line fitting method: \describe{ \item{'SMA' or
#' 1}{standardised major axis (this is the default)} \item{'MA' or 2}{major
#' axis} }
#' @param intercept (logical) Whether or not the line includes an intercept.
#' \describe{ \item{FALSE}{ no intercept, so the line is forced through the
#' origin } \item{TRUE}{ an intercept is fitted (this is the default) } }
#' @param robust If TRUE, uses a robust method to fit the lines and construct
#' the test statistic.
#' @param V The estimated variance matrices of measurement error, for each
#' group. This is a 3-dimensional array with measurement error in Y in the
#' first row and column, error in X in the second row and column, and groups
#' running along the third dimension. Default is that there is no measurement
#' error.
#' @param group.names (optional: rarely required). A vector containing the
#' labels for `groups'. (Only actually useful for reducing computation time in
#' simulation work).
#' @return \item{stat}{The Wald statistic testing for no shift along the common
#' axis} \item{p}{The P-value of the test. This is calculated assuming that
#' stat has a chi-square distribution with (g-1) df, if there are g groups}
#' \item{f.mean}{The fitted axis means for each group}
#' @author Warton, D.I.\email{David.Warton@@unsw.edu.au}, J. Ormerod, & S.
#' Taskinen
#' @seealso \code{\link{sma}}, \code{\link{plot.sma}}, \code{\link{line.cis}},
#' \code{\link{elev.com}}, \code{\link{shift.com}}
#' @references Warton D. I., Wright I. J., Falster D. S. and Westoby M. (2006)
#' A review of bivariate line-fitting methods for allometry.  \emph{Biological
#' Reviews} \bold{81}, 259--291.
#' 
#' Taskinen, S. and D.I. Warton. in review. Robust tests for one or more
#' allometric lines.
#' @keywords htest
#' @examples
#' 
#' #load leaf longevity data
#' data(leaflife)
#' 
#' #Test for common SMA slope amongst species at low rainfall sites
#' #with different levels of soil nutrients
#' leaf.low.rain=leaflife[leaflife$rain=='low',]
#' with(leaf.low.rain, slope.com(log10(longev), log10(lma), soilp))
#' 
#' #Now test for no shift along the axes of common slope, for sites
#' #with different soil nutrient levels but low rainfall:
#' with(leaf.low.rain, shift.com(log10(longev), log10(lma), soilp))
#' 
#' #Now test for no shift along the axes of common slope, for sites
#' #with different soil nutrient levels but low rainfall:
#' with(leaf.low.rain,shift.com(log10(longev), log10(lma), soilp, method='MA'))
#' 
#' 
shift.com <- function( y, x, groups, data=NULL, method="SMA", intercept=TRUE, robust=FALSE ,  V=array( 0, c( 2,2,length(unique(groups)) ) ), group.names=sort(unique(groups)))
{
#     if ( is.null(data)==FALSE )
#     {
#         attach(data)
#     }

  
  if(!is.null(data))
    stop("'data' argument no longer supported.")
  
  
    y <- as.matrix(y)
    x <- as.matrix(x)
    dat    <- cbind(y, x)
    groups <- as.matrix(groups)

    nobs <- length(groups)
    g    <- length(group.names)
    inter<- intercept

    res  <- slope.com( y, x, groups, method, intercept=inter, V=V, ci=FALSE, bs=FALSE, robust=robust )
    lr   <- res$lr
    p    <- res$p
    b    <- res$b
    varb <- res$varb

    n        <- matrix( 0, g, 1 )
    varAxis  <- n
    as       <- n
    means    <- matrix( 0, g, 2 )

    if ( (method=="SMA") | method==1 )
    {
        axis       <- y + b*x
        coefV1     <- 1 #The coef of V[1,1,:] in var(axis).
        coefV2     <- b^2 #The coef of V[2,2,:] in var(axis).
        mean.ref   <- 2 #Ref for the column of means to use as coef of var(b)
    }
    if ( (method=="MA") | method==2 )
    {
        axis       <- b*y + x
        coefV1     <- b^2 #The coef of V[1,1,:] in var(axis).
        coefV2     <- 1
        mean.ref   <- 1 #Ref for the column of means to use as coef of var(b)
    }
 
    for ( i in 1:g )
    {
        iref       <- ( groups==group.names[i] )
        iref       <- iref & ( is.na(x+y) == FALSE )
        n[i]       <- sum( iref )
     
        if (robust)       
        {
		q          <- pchisq(3,2)
                S          <- huber.M(dat[iref,])
                r.mean     <- S$loc

             	# robust factor for means:
                r.factor2 <- robust.factor(dat[iref,],q)[2]

        	means[i,1] <- r.mean[1] 
        	means[i,2] <- r.mean[2]	
                
                if ( (method=="SMA") | method==1 )
                {
			as[i]      <- means[i,1] + b*means[i,2]				
			varAxis[i] <- (S$cov[1,1] + 2*b*S$cov[1,2] + b^2*S$cov[2,2]) * r.factor2
                } 

 		if ( (method=="MA") | method==2 )
    		{
			as[i]      <- b * means[i,1] + means[i,2]				
			varAxis[i] <- (b^2 * S$cov[1,1] + 2*b*S$cov[1,2] + S$cov[2,2]) * r.factor2
 		}
        }
        else
        {
		means[i,1] <- mean( y[iref] ) 
       	      	means[i,2] <- mean( x[iref] )
       	       	as[i]      <- mean( axis[iref] )
       	       	varAxis[i] <- var( axis[iref] )
        }

    }
    varAxis    <- varAxis - coefV1*V[1,1,] - coefV2*V[2,2,]
    varAxis    <- varAxis * (n-1) / (n-2)
    mean.for.b <- means[,mean.ref]

    varAs <- diag( array(varAxis/n) ) + varb*mean.for.b%*%t(mean.for.b)

    varAs[n==1,] <- 0 #For singleton groups
    varAs[,n==1] <- 0
    df     <- g - 1 - sum(n==1)
    L      <- matrix(0,df,g)
    L[,n>1] <- cbind( matrix( 1, df, 1), diag( array( -1, df), nrow=df ) )
    stat  <- t(L%*%as)%*%solve(L%*%varAs%*%t(L), tol=1.0e-050 )%*%(L%*%as)

    pvalue <- 1 - pchisq( stat, df )

#     if ( is.null(data)==FALSE )
#     {
#         detach(data)
#     }

    list( stat=stat, p=pvalue, f.mean=as.vector(as), df=df )

}
