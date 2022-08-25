#' Test for equal elevation among several lines fitted with (standardised)
#' major axes of common slope.
#' 
#' @description Test if several major axis or standardised major axis lines share a common
#' elevation.  This can now be done via \code{sma(y~x+groups)}, see help on the
#' \code{\link{sma}} function.
#' 
#' @details Calculates a Wald statistic to test for equal elevation of several MA's or
#' SMA's with a common slope. This is done by testing for equal mean residual
#' scores across groups.
#' 
#' Note that this test is only valid if it is reasonable to assume that the
#' axes for the different groups all have the same slope.
#' 
#' The test assumes the following: \itemize{ \item each group of observations
#' was independently sampled \item the axes fitted to all groups have a common
#' slope \item y and x are linearly related within each group \item residual
#' scores independently follow a normal distribution with equal variance at all
#' points along the line, within each group } Note that we do not need to
#' assume equal variance across groups, unlike in tests comparing several
#' linear regression lines.
#' 
#' The assumptions can be visually checked by plotting residual scores against
#' fitted axis scores, and by constructing a Q-Q plot of residuals against a
#' normal distribution, available using the \code{\link{plot.sma}} function. On
#' a residual plot, if there is a distinct increasing or decreasing trend
#' within any of the groups, this suggests that all groups do not share a
#' common slope.
#' 
#' Setting \code{robust=TRUE} fits lines using Huber's M estimation, and
#' modifies the test statistic as proposed in Taskinen & Warton (in review).
#' 
#' The common slope (\eqn{\hat{\beta}}{b}) is estimated from a maximum of 100
#' iterations, convergence is reached when the change in \eqn{\hat{\beta} <
#' 10^{-6}}{b < 10^-6}.
#' 
#' @param y The Y-variable for all observations (as a vector).
#' @param x The X-variable for all observations (as a vector).
#' @param groups Coding variable identifying which group each observation
#' belongs to (as a factor or vector).
#' @param data Deprecated. Use with() instead (see Examples).
#' @param method The line fitting method: \describe{ \item{'SMA' or 1}{
#' standardised major axis (this is the default) } \item{'MA' or 2}{ major axis
#' } }
#' @param alpha The desired confidence level for the 100(1-alpha)\% confidence
#' interval for the common elevation. (Default value is 0.05, which returns a
#' 95\% confidence interval.)
#' @param robust If TRUE, uses a robust method to fit the lines and construct
#' the test statistic.
#' @param V The estimated variance matrices of measurement error, for each
#' group. This is a 3-dimensional array with measurement error in Y in the
#' first row and column, error in X in the second row and column,and groups
#' running along the third dimension. Default is that there is no measurement
#' error.
#' @param group.names (optional: rarely required). A vector containing the
#' labels for `groups'. (Only actually useful for reducing computation time in
#' simulation work).
#' @return \item{stat }{The Wald statistic testing for no shift along the
#' common axis} \item{p }{The P-value of the test. This is calculated assuming
#' that stat has a chi-square distribution with (g-1) df, if there are g
#' groups} \item{a }{The estimated common elevation} \item{ci }{A
#' 100(1-alpha)\% confidence interval for the true common elevation} \item{as
#' }{Separate elevation estimates for each group}
#' @author Warton, D.I.\email{David.Warton@@unsw.edu.au}, J. Ormerod, & S.
#' Taskinen
#' @seealso \code{\link{sma}}, \code{\link{plot.sma}}, \code{\link{line.cis}},
#' \code{\link{slope.com}}, \code{\link{shift.com}}
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
#' # Load leaf longevity data
#' data(leaflife)
#' 
#' # Test for common SMA slope amongst species at low soil nutrient sites
#' # with different rainfall:
#' leaf.low.soilp <- subset(leaflife, soilp == 'low')
#' with(leaf.low.soilp, slope.com(log10(longev), log10(lma), rain))
#' 
#' # Now test for common elevation of the groups fitted with an axis
#' # of common slope, at low soil nutrient sites:
#' with(leaf.low.soilp, elev.com(log10(longev), log10(lma), rain))
#' 
#' # Or test for common elevation amongst the MA's of common slope,
#' # for low soil nutrient sites, and construct 99% a confidence interval
#' # for the common elevation:
#' with(leaf.low.soilp, elev.com(log10(longev), log10(lma), rain, method='MA',
#'    alpha=0.01))
#' 
elev.com <- function( y, x, groups, data=NULL, method="SMA", alpha=0.05, robust=FALSE, V=array( 0, c( 2,2,length(unique(groups)) ) ), 
    group.names=sort(unique(groups)) )
{
#     if ( is.null(data)==FALSE )
#     {
#         attach(data)
#     }

    if(!is.null(data)){
      stop("'data' argument no longer supported. Use with() instead.")
    }
  
    x      <- as.matrix( x )
    y      <- as.matrix( y )
    dat    <- cbind(y, x)
    groups <- as.matrix( groups )

    nobs <- length( groups )
    g    <- length( group.names )
    res  <- slope.com( y, x, groups, method=method, V=V, bs=FALSE, ci=FALSE, robust=robust )   
    lr   <- res$lr
    p    <- res$p
    b    <- res$b
    varb <- res$varb

    n      <- matrix( 0, g, 1 )
    varres <- matrix( 0, g, 1 )
    means  <- matrix( 0, g, 2 )
    res    <- y - b*x

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

                varres[i] <- ((S$cov[1,1] - 2*b*S$cov[1,2] + b^2*S$cov[2,2]) * r.factor2 - V[1,1,i] - b^2*V[2,2,i] )
        }
        else
        {
		means[i,1] <- mean( y[iref] ) 
        	means[i,2] <- mean( x[iref] )
        	varres[i]  <- ( var( res[iref] ) - V[1,1,i] - b^2*V[2,2,i] )
        }
    }

    varres <- varres *( n - 1 )/( n - 2 )

    as     <- means[,1] - b*means[,2]
    names(as) <- group.names
    varas  <- diag( array( varres/n ) ) + varb * means[,2]%*%t( means[,2] )

    varas[n==1,] <- 0 #For singleton groups
    varas[,n==1] <- 0
    df     <- g - 1 - sum(n==1)
    L      <- matrix(0,df,g)
    L[,n>1] <- cbind( matrix( 1, df, 1), diag( array( -1, df), nrow=df ) )
    
    stat   <- t(L%*%as)%*%( solve( L%*%varas%*%t(L) ) )%*%(L%*%as)
    pvalue <- 1 - pchisq( stat, df ) 
    sinv=matrix(0,g,g)
    sinv[n>1,n>1]   <- solve( varas[n>1,n>1] )
    a      <- (matrix(1,1,g)%*%sinv%*%as)/ sum( sum( sinv) )
    vara   <- 1 / sum( sum( sinv ) )
    crit   <- qchisq( 1 - alpha, 1 )
    crits  <- qt( 1 - alpha/2, n-2 )
    a.ci   <- c( a - sqrt( crit*vara), a + sqrt( crit*vara ) )
    as.ci  <- as + cbind(0, - crits*sqrt(diag(varas)), crits*sqrt(diag(varas)) )
	
    dimnames(as.ci)[[1]] = group.names
    dimnames(as.ci)[[2]] <- c("elevation","lower CI limit","upper CI limit")
# 
#     if ( is.null(data)==FALSE )
#     {
#         detach(data)
#     }

    list( stat=stat, p=pvalue , a=a, ci=a.ci, as = as.ci, df=df )
}
